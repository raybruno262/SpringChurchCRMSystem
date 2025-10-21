package com.SpringChurchCRMSystem.SpringChurchCRMSystem.Dto;

public class EquipmentStatsDTO {
    private long totalEquipment;
    private long excellentCount;
    private long goodCount;
    private long needsRepairCount;
    private long outOfServiceCount;

    public EquipmentStatsDTO(long totalEquipment, long excellentCount, long goodCount,
            long needsRepairCount, long outOfServiceCount) {
        this.totalEquipment = totalEquipment;
        this.excellentCount = excellentCount;
        this.goodCount = goodCount;
        this.needsRepairCount = needsRepairCount;
        this.outOfServiceCount = outOfServiceCount;
    }

    // Getters and setters
    public long getTotalEquipment() {
        return totalEquipment;
    }

    public void setTotalEquipment(long totalEquipment) {
        this.totalEquipment = totalEquipment;
    }

    public long getExcellentCount() {
        return excellentCount;
    }

    public void setExcellentCount(long excellentCount) {
        this.excellentCount = excellentCount;
    }

    public long getGoodCount() {
        return goodCount;
    }

    public void setGoodCount(long goodCount) {
        this.goodCount = goodCount;
    }

    public long getNeedsRepairCount() {
        return needsRepairCount;
    }

    public void setNeedsRepairCount(long needsRepairCount) {
        this.needsRepairCount = needsRepairCount;
    }

    public long getOutOfServiceCount() {
        return outOfServiceCount;
    }

    public void setOutOfServiceCount(long outOfServiceCount) {
        this.outOfServiceCount = outOfServiceCount;
    }
}