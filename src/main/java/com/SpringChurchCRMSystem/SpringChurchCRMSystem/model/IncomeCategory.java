package com.SpringChurchCRMSystem.SpringChurchCRMSystem.model;

import org.springframework.data.annotation.Id;
import org.springframework.data.mongodb.core.mapping.Document;

@Document(collection = "income_category")
public class IncomeCategory {
    @Id
    private String incomeCategoryId;
    private String name;

    public IncomeCategory() {
    }

    public IncomeCategory(String incomeCategoryId) {
        this.incomeCategoryId = incomeCategoryId;
    }

    public IncomeCategory(String incomeCategoryId, String name) {
        this.incomeCategoryId = incomeCategoryId;
        this.name = name;
    }

    public String getIncomeCategoryId() {
        return incomeCategoryId;
    }

    public void setIncomeCategoryId(String incomeCategoryId) {
        this.incomeCategoryId = incomeCategoryId;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

}