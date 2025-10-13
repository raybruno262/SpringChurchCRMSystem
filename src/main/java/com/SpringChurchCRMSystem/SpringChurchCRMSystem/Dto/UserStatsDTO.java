package com.SpringChurchCRMSystem.SpringChurchCRMSystem.Dto;

public class UserStatsDTO {
    private long totalUsers;
    private long activeUsers;
    private long inactiveusers;

    public UserStatsDTO() {
    }

    public UserStatsDTO(long totalUsers, long activeUsers, long inactiveusers) {
        this.totalUsers = totalUsers;
        this.activeUsers = activeUsers;
        this.inactiveusers = inactiveusers;
    }

    public long getTotalUsers() {
        return totalUsers;
    }

    public void setTotalUsers(long totalUsers) {
        this.totalUsers = totalUsers;
    }

    public long getActiveUsers() {
        return activeUsers;
    }

    public void setActiveUsers(long activeUsers) {
        this.activeUsers = activeUsers;
    }

    public long getInactiveusers() {
        return inactiveusers;
    }

    public void setInactiveusers(long inactiveusers) {
        this.inactiveusers = inactiveusers;
    }

}
