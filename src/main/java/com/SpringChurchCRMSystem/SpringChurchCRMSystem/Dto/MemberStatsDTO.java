package com.SpringChurchCRMSystem.SpringChurchCRMSystem.Dto;

public class MemberStatsDTO {
    private long totalMembers;
    private long activeMembers;
    private long inactiveMembers;
    private long transferredMembers;

    public MemberStatsDTO(long total, long active, long inactive, long transferred) {
        this.totalMembers = total;
        this.activeMembers = active;
        this.inactiveMembers = inactive;
        this.transferredMembers = transferred;
    }

    // Getters and setters
    public long getTotalMembers() {
        return totalMembers;
    }

    public long getActiveMembers() {
        return activeMembers;
    }

    public long getInactiveMembers() {
        return inactiveMembers;
    }

    public long getTransferredMembers() {
        return transferredMembers;
    }
}
