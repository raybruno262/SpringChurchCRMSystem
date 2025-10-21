package com.SpringChurchCRMSystem.SpringChurchCRMSystem.Dto;

public class FinanceStatsDTO {
    private double totalIncome;
    private double totalExpenses;
    private double currentBalance;

    public FinanceStatsDTO(double totalIncome, double totalExpenses, double currentBalance) {
        this.totalIncome = totalIncome;
        this.totalExpenses = totalExpenses;
        this.currentBalance = currentBalance;
    }

    // Getters and setters
    public double getTotalIncome() {
        return totalIncome;
    }

    public void setTotalIncome(double totalIncome) {
        this.totalIncome = totalIncome;
    }

    public double getTotalExpenses() {
        return totalExpenses;
    }

    public void setTotalExpenses(double totalExpenses) {
        this.totalExpenses = totalExpenses;
    }

    public double getCurrentBalance() {
        return currentBalance;
    }

    public void setCurrentBalance(double currentBalance) {
        this.currentBalance = currentBalance;
    }
}