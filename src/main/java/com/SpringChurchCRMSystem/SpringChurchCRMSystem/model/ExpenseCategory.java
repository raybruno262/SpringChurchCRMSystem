package com.SpringChurchCRMSystem.SpringChurchCRMSystem.model;

import org.springframework.data.annotation.Id;
import org.springframework.data.mongodb.core.mapping.Document;

@Document(collection = "expense_category")
public class ExpenseCategory {
    @Id
    private String expenseCategoryId;
    private String name;

    public ExpenseCategory() {
    }

    public ExpenseCategory(String expenseCategoryId) {
        this.expenseCategoryId = expenseCategoryId;
    }

    public ExpenseCategory(String expenseCategoryId, String name) {
        this.expenseCategoryId = expenseCategoryId;
        this.name = name;
    }

    public String getExpenseCategoryId() {
        return expenseCategoryId;
    }

    public void setExpenseCategoryId(String expenseCategoryId) {
        this.expenseCategoryId = expenseCategoryId;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

}