library(plumber)
library(jsonlite)
library(httr)
library(lubridate)
library(dplyr)
library(tidyr)
library(purrr)
library(tibble)

FIREBASE_URL <- "https://walkmate-5bbcc-default-rtdb.firebaseio.com"
FIREBASE_SECRET <- "YOUR_SECRET_KEY"

# 시간 포맷 변환
format_time_hm <- function(total_seconds) {
  if (is.na(total_seconds) || total_seconds == 0) return("0분")
  total_minutes <- floor(total_seconds / 60)
  h <- floor(total_minutes / 60)
  m <- total_minutes %% 60
  if (h > 0) paste0(h,"시간 ",m,"분") else paste0(m,"분")
}

# 사용자 목표 가져오기
get_user_goal <- function(uid, auth_token) {
  url <- paste0(FIREBASE_URL, "/users/", uid, ".json?auth=", auth_token)
  tryCatch({
    res <- GET(url)
    user <- content(res, "parsed")
    if (is.null(user)) return(42.0)
    target <- user$goals$weekly_distance_km
    if (!is.null(target) && target > 0) return(target)
    return(42.0)
  }, error=function(e) 42.0)
}

# 일별 데이터 가져오기
get_daily_walks <- function(uid, auth_token) {
  url <- paste0(FIREBASE_URL, "/dailyWalks/", uid, ".json?auth=", auth_token)
  tryCatch({
    res <- GET(url)
    data <- content(res, "parsed")
    if (is.null(data)) return(list())
    return(data)
  }, error=function(e) list())
}

get_recent_4month_avg_firebase <- function(daily_walks) {
  if (length(daily_walks) == 0) return(NULL)

  df <- tibble::enframe(daily_walks, name="date_str", value="m") %>%
    unnest_wider(m) %>%
    mutate(
      date = ymd(date_str),
      distance_km = as.numeric(totalDistanceKm),
      walk_min = as.numeric(totalTimeSeconds) / 60
    ) %>%
    select(date, distance_km, walk_min)

  latest_month <- floor_date(max(df$date), "month")
  months_full <- sort(latest_month %m-% months(0:3))
  month_labels <- paste0(month(months_full), "월")

  df_avg <- df %>%
    mutate(month_start = floor_date(date, "month")) %>%
    filter(month_start %in% months_full) %>%
    group_by(month_start) %>%
    summarise(
      month = paste0(month(first(month_start)), "월"),
      avg_distance = round(mean(distance_km, na.rm=TRUE), 2),
      avg_time = round(mean(walk_min, na.rm=TRUE), 1),
      .groups="drop"
    )

  tibble(month = factor(month_labels, levels=month_labels)) %>%
    left_join(df_avg, by="month") %>%
    mutate(
      avg_distance = replace_na(avg_distance, 0),
      avg_time = replace_na(avg_time, 0)
    )
}

#* @get /weekly_stats
#* @serializer unboxedJSON
function(user_id="") {
  tryCatch({
    if (user_id == "") return(list(error="user_id 필요"))

    goal <- get_user_goal(user_id, FIREBASE_SECRET)
    daily <- get_daily_walks(user_id, FIREBASE_SECRET)

    today <- Sys.Date()
    week_start <- floor_date(today, "week", week_start=1)
    week_end <- ceiling_date(today, "week", week_start=1) - days(1)

    total_dist <- 0
    total_sec <- 0
    daily_list <- list()

    for (d in names(daily)) {
      dt <- as.Date(d)
      if (dt >= week_start && dt <= week_end) {
        info <- daily[[d]]
        dist <- as.numeric(info$totalDistanceKm)
        sec <- as.numeric(info$totalTimeSeconds)
        total_dist <- total_dist + dist
        total_sec <- total_sec + sec
        daily_list <- c(daily_list, list(list(
          date=d, walk_min=floor(sec/60), distance_km=round(dist,2)
        )))
      }
    }

    list(
      user_id=user_id,
      week_start=as.character(week_start),
      week_end=as.character(week_end),
      total_walk_time=format_time_hm(total_sec),
      total_distance_km=round(total_dist,2),
      goal_distance_km=goal,
      achieve_rate_percent=round(total_dist/goal*100,1),
      graph_base64="",
      daily=daily_list
    )
  }, error=function(e) list(error=e$message))
}

#* @get /monthly_avg_stats
#* @serializer unboxedJSON
function(user_id="") {
  tryCatch({
    if (user_id == "") return(list(error="user_id 필요"))

    daily <- get_daily_walks(user_id, FIREBASE_SECRET)
    df <- get_recent_4month_avg_firebase(daily)

    if (is.null(df)) {
      return(list(
        user_id=user_id,
        months=list(),
        avg_distance_km=list(),
        avg_time_min=list()
      ))
    }

    list(
      user_id=user_id,
      months = as.list(as.character(df$month)),
      avg_distance_km = as.list(df$avg_distance),
      avg_time_min = as.list(df$avg_time)
    )
  }, error=function(e) list(error=e$message))
}
