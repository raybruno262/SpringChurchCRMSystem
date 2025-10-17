package com.SpringChurchCRMSystem.SpringChurchCRMSystem.model;

import java.time.LocalDate;

import org.springframework.data.annotation.Id;
import org.springframework.data.mongodb.core.mapping.DBRef;
import org.springframework.data.mongodb.core.mapping.Document;

import com.fasterxml.jackson.annotation.JsonFormat;

@Document(collection = "visitors")
public class Visitor {
    @Id
    private String visitorId;
    private String names;
    private String phone;
    private String gender;
    private String email;
    private String address;
    @JsonFormat(pattern = "MM/dd/yyyy")
    private LocalDate visitDate;
    private String status; // new , follow-up, converted, dropped
    private FollowUp followUp;
    @DBRef
    private Level level;

    public Visitor() {
    }

    public Visitor(String visitorId) {
        this.visitorId = visitorId;
    }

    public Visitor(String visitorId, String names, String phone, String gender, String email, String address,
            LocalDate visitDate, String status, FollowUp followUp, Level level) {
        this.visitorId = visitorId;
        this.names = names;
        this.phone = phone;
        this.gender = gender;
        this.email = email;
        this.address = address;
        this.visitDate = visitDate;
        this.status = status;
        this.followUp = followUp;
        this.level = level;
    }

    public String getVisitorId() {
        return visitorId;
    }

    public void setVisitorId(String visitorId) {
        this.visitorId = visitorId;
    }

    public String getNames() {
        return names;
    }

    public void setNames(String names) {
        this.names = names;
    }

    public String getPhone() {
        return phone;
    }

    public void setPhone(String phone) {
        this.phone = phone;
    }

    public String getGender() {
        return gender;
    }

    public void setGender(String gender) {
        this.gender = gender;
    }

    public String getEmail() {
        return email;
    }

    public void setEmail(String email) {
        this.email = email;
    }

    public String getAddress() {
        return address;
    }

    public void setAddress(String address) {
        this.address = address;
    }

    public LocalDate getVisitDate() {
        return visitDate;
    }

    public void setVisitDate(LocalDate visitDate) {
        this.visitDate = visitDate;
    }

    public String getStatus() {
        return status;
    }

    public void setStatus(String status) {
        this.status = status;
    }

    public FollowUp getFollowUp() {
        return followUp;
    }

    public void setFollowUp(FollowUp followUp) {
        this.followUp = followUp;
    }

    public Level getLevel() {
        return level;
    }

    public void setLevel(Level level) {
        this.level = level;
    }

}