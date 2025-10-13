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
    @JsonFormat(pattern = "MM/dd/yyyy")
    private LocalDate membershipDate;

    private BaptismInformation baptismInformation; // Baptism Object
    private byte[] profilePic;

    @DBRef
    private Department department;
    @DBRef
    private Level level;

    public Member() {
    }

    public Member(String memberId) {
        this.memberId = memberId;
    }

    public Member(String memberId, String names, LocalDate dateOfBirth, String phone, String gender,
            String maritalStatus, String email, String status, String address, LocalDate membershipDate,
            BaptismInformation baptismInformation, byte[] profilePic, Department department, Level level) {
        this.memberId = memberId;
        this.names = names;
        this.dateOfBirth = dateOfBirth;
        this.phone = phone;
        this.gender = gender;
        this.maritalStatus = maritalStatus;
        this.email = email;
        this.status = status;
        this.address = address;
        this.membershipDate = membershipDate;
        this.baptismInformation = baptismInformation;
        this.profilePic = profilePic;
        this.department = department;
        this.level = level;
    }

    public String getMemberId() {
        return memberId;
    }

    public void setMemberId(String memberId) {
        this.memberId = memberId;
    }

    public String getNames() {
        return names;
    }

    public void setNames(String names) {
        this.names = names;
    }

    public LocalDate getDateOfBirth() {
        return dateOfBirth;
    }

    public void setDateOfBirth(LocalDate dateOfBirth) {
        this.dateOfBirth = dateOfBirth;
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

    public String getMaritalStatus() {
        return maritalStatus;
    }

    public void setMaritalStatus(String maritalStatus) {
        this.maritalStatus = maritalStatus;
    }

    public String getEmail() {
        return email;
    }

    public void setEmail(String email) {
        this.email = email;
    }

    public String getStatus() {
        return status;
    }

    public void setStatus(String status) {
        this.status = status;
    }

    public String getAddress() {
        return address;
    }

    public void setAddress(String address) {
        this.address = address;
    }

    public LocalDate getMembershipDate() {
        return membershipDate;
    }

    public void setMembershipDate(LocalDate membershipDate) {
        this.membershipDate = membershipDate;
    }

    public BaptismInformation getBaptismInformation() {
        return baptismInformation;
    }

    public void setBaptismInformation(BaptismInformation baptismInformation) {
        this.baptismInformation = baptismInformation;
    }

    public byte[] getProfilePic() {
        return profilePic;
    }

    public void setProfilePic(byte[] profilePic) {
        this.profilePic = profilePic;
    }

    public Department getDepartment() {
        return department;
    }

    public void setDepartment(Department department) {
        this.department = department;
    }

    public Level getLevel() {
        return level;
    }

    public void setLevel(Level level) {
        this.level = level;
    }

}
