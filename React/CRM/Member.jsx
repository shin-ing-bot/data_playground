import React from "react";
import MemberItem from "./Memberitem";

function MemberList({ members, deleteMember, editMember }) {
    return (
        <div>
            {members.length === 0 ? (
                <p>회원이 없습니다.</p>
            ) : (
                members.map((member) => (
                    <MemberItem
                        key={member.id}
                        member={member}
                        deleteMember={deleteMember}
                        editMember={editMember}
                    />
                ))
            )}
        </div>
    );
}


export default MemberList;
