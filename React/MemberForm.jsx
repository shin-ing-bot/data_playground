import React, { useEffect, useState } from 'react';

function MemberForm({
    addMember,
    editingMember,
    updateMember,
}) {
    const [name, setName] = useState("");
    const [age, setAge] = useState("");
    const [email, setEmail] = useState("");

    
    useEffect(() => {
        if (editingMember) {
            setName(editingMember.name);
            setAge(editingMember.age);
            setEmail(editingMember.email);
        }
    }, [editingMember]);

    const handleSubmit = (e) => {
        e.preventDefault();

     
        if (!name || !age || !email) {
            alert("모든 항목 입력");
            return; 
        }

        if (editingMember) {
            // 수정 모드
            updateMember({
                id: editingMember.id,
                name,
                age: Number(age),
                email
            });
        } else {
            // 추가 모드
            addMember({
                id: Date.now(),
                name,
                age: Number(age),
                email,
            });
        }

       
        setName("");
        setAge("");
        setEmail("");
    }; 

   
    return (
        <form className="form" onSubmit={handleSubmit}>
            <input
                type='text'
                placeholder='이름'
                value={name}
                onChange={(e) => setName(e.target.value)}
            />

            <input
                type='number'
                placeholder='나이'
                value={age}
                onChange={(e) => setAge(e.target.value)}
            />
            
            <input
                type='email'
                placeholder='이메일'
                value={email}
                onChange={(e) => setEmail(e.target.value)}
            />

            <button type='submit'>
                {editingMember ? "회원 수정" : "회원 추가"}
            </button>
        </form>
    );
};

export default MemberForm;