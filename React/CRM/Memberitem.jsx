import React from "react";

function MemberItem({
    member,
    deleteMember,
    editMember
}) {
    return (
        <div className="member-card">
            <h3>{member.name}</h3>

            <p>나이 :{member.age} </p>

            <p>이메일 : {member.email}</p>

            <div className="button-group">
                <button
                className="edit-btn"
                onClick={()=> editMember(member)}
                >
                    수정
                </button>

                <button
                className="delete-btn"
                onClick={() => deleteMember(member.id)}
                >
                    삭제
                </button>
            </div>
            
        </div>
    );
}
export default MemberItem;
