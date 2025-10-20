package com.SpringChurchCRMSystem.SpringChurchCRMSystem.Dto;

public class VisitorStatsDTO {
    private long totalVisitors;
    private long newVisitors;
    private long followedUpVisitors;
    private long convertedVisitors;
    private long droppedVisitors;

    public VisitorStatsDTO() {
    }

    public VisitorStatsDTO(long totalVisitors, long newVisitors, long followedUpVisitors, long convertedVisitors,
            long droppedVisitors) {
        this.totalVisitors = totalVisitors;
        this.newVisitors = newVisitors;
        this.followedUpVisitors = followedUpVisitors;
        this.convertedVisitors = convertedVisitors;
        this.droppedVisitors = droppedVisitors;
    }

    public long getTotalVisitors() {
        return totalVisitors;
    }

    public void setTotalVisitors(long totalVisitors) {
        this.totalVisitors = totalVisitors;
    }

    public long getNewVisitors() {
        return newVisitors;
    }

    public void setNewVisitors(long newVisitors) {
        this.newVisitors = newVisitors;
    }

    public long getFollowedUpVisitors() {
        return followedUpVisitors;
    }

    public void setFollowedUpVisitors(long followedUpVisitors) {
        this.followedUpVisitors = followedUpVisitors;
    }

    public long getConvertedVisitors() {
        return convertedVisitors;
    }

    public void setConvertedVisitors(long convertedVisitors) {
        this.convertedVisitors = convertedVisitors;
    }

    public long getDroppedVisitors() {
        return droppedVisitors;
    }

    public void setDroppedVisitors(long droppedVisitors) {
        this.droppedVisitors = droppedVisitors;
    }

}
