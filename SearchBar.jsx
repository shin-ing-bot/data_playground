import React from "react";

function SearchBar({search, setSearch}) {
    
    return (
        <input
        className="seach-input"
        type="text"
        placeholder="회원 검색"
        value={search}
        onChange={(e) => setSearch(e.target.value)}
        ></input>
    );
}
export default SearchBar;