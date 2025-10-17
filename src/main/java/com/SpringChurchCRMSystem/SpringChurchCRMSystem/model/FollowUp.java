package com.SpringChurchCRMSystem.SpringChurchCRMSystem.model;

import java.time.LocalDate;

import com.fasterxml.jackson.annotation.JsonFormat;

public class FollowUp {
    @JsonFormat(pattern = "MM/dd/yyyy")
    private LocalDate followUpDate;
    private String method; // Call, Visit, SMS, WhatsApp
    private String outcome; // Interested, Needs Prayer, Converted, Not Interested
    private String notes; // description of the outcome
    private String followedUpBy; // Name

    public FollowUp() {
    }

    public FollowUp(LocalDate followUpDate, String method, String outcome, String notes, String followedUpBy) {
        this.followUpDate = followUpDate;
        this.method = method;
        this.outcome = outcome;
        this.notes = notes;
        this.followedUpBy = followedUpBy;
    }

    public LocalDate getFollowUpDate() {
        return followUpDate;
    }

    public void setFollowUpDate(LocalDate followUpDate) {
        this.followUpDate = followUpDate;
    }

    public String getMethod() {
        return method;
    }

    public void setMethod(String method) {
        this.method = method;
    }

    public String getOutcome() {
        return outcome;
    }

    public void setOutcome(String outcome) {
        this.outcome = outcome;
    }

    public String getNotes() {
        return notes;
    }

    public void setNotes(String notes) {
        this.notes = notes;
    }

    public String getFollowedUpBy() {
        return followedUpBy;
    }

    public void setFollowedUpBy(String followedUpBy) {
        this.followedUpBy = followedUpBy;
    }

}
