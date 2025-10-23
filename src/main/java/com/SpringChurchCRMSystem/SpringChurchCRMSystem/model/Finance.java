package com.SpringChurchCRMSystem.SpringChurchCRMSystem.model;

import java.time.LocalDate;

import org.springframework.data.annotation.Id;
import org.springframework.data.mongodb.core.mapping.DBRef;
import org.springframework.data.mongodb.core.mapping.Document;

@Document(collection = "finance")
public class Finance {
    @Id
    private String financeId;
    @DBRef
    private IncomeCategory incomeCategory;
    @DBRef
    private ExpenseCategory expenseCategory;

    private LocalDate transactionDate;
    private double amount;
    private String transactionType;
    private String description;

    @DBRef
    private Level level;

    public Finance() {
    }

    public Finance(String financeId) {
        this.financeId = financeId;
    }

    public Finance(String financeId, IncomeCategory incomeCategory, ExpenseCategory expenseCategory,
            LocalDate transactionDate, double amount, String transactionType, String description, Level level) {
        this.financeId = financeId;
        this.incomeCategory = incomeCategory;
        this.expenseCategory = expenseCategory;
        this.transactionDate = transactionDate;
        this.amount = amount;
        this.transactionType = transactionType;
        this.description = description;
        this.level = level;
    }

    public String getFinanceId() {
        return financeId;
    }

    public void setFinanceId(String financeId) {
        this.financeId = financeId;
    }

    public IncomeCategory getIncomeCategory() {
        return incomeCategory;
    }

    public void setIncomeCategory(IncomeCategory incomeCategory) {
        this.incomeCategory = incomeCategory;
    }

    public ExpenseCategory getExpenseCategory() {
        return expenseCategory;
    }

    public void setExpenseCategory(ExpenseCategory expenseCategory) {
        this.expenseCategory = expenseCategory;
    }

    public LocalDate getTransactionDate() {
        return transactionDate;
    }

    public void setTransactionDate(LocalDate transactionDate) {
        this.transactionDate = transactionDate;
    }

    public double getAmount() {
        return amount;
    }

    public void setAmount(double amount) {
        this.amount = amount;
    }

    public String getTransactionType() {
        return transactionType;
    }

    public void setTransactionType(String transactionType) {
        this.transactionType = transactionType;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public Level getLevel() {
        return level;
    }

    public void setLevel(Level level) {
        this.level = level;
    }

}
