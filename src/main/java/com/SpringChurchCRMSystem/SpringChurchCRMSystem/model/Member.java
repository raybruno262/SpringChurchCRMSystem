package com.SpringChurchCRMSystem.SpringChurchCRMSystem.model;

import java.time.LocalDate;

import org.springframework.data.annotation.Id;
import org.springframework.data.mongodb.core.mapping.DBRef;
import org.springframework.data.mongodb.core.mapping.Document;

import com.fasterxml.jackson.annotation.JsonFormat;

@Document(collection = "members")
public class Member {
    @Id
    private String memberId;
    private String names;

    @JsonFormat(pattern = "MM/dd/yyyy")
    private LocalDate dateOfBirth;

    private String phone;
    private String gender; // Male or Female
    private String maritalStatus; // Single or Married
    private String email;
    private String status;// Active, Inactive or Transferred

    private String address;
    private LocalDate membershipDate;

    private String baptismStatus; // baptized, not baptized

    @DBRef
    private Department department;
    @DBRef
    private Level level;

    public Member() {
    }

    public Member(String memberId) {
        this.memberId = memberId;
    }

}
