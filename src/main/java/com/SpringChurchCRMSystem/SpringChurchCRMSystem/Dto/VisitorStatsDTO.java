package com.SpringChurchCRMSystem.SpringChurchCRMSystem.Dto;

public class VisitorStatsDTO {
    private long totalVisitors;
    private long newVisitors;
    private long followedUpVisitors;
    private long convertedVisitors;

    public VisitorStatsDTO(long total, long newVisitors, long followedUp, long converted) {
        this.totalVisitors = total;
        this.newVisitors = newVisitors;
        this.followedUpVisitors = followedUp;
        this.convertedVisitors = converted;
    }

    // Getters and setters
    public long getTotalVisitors() {
        return totalVisitors;
    }

    public long getNewVisitors() {
        return newVisitors;
    }

    public long getFollowedUpVisitors() {
        return followedUpVisitors;
    }

    public long getConvertedVisitors() {
        return convertedVisitors;
    }
}
