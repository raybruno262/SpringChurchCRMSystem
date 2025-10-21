package com.SpringChurchCRMSystem.SpringChurchCRMSystem.model;

import org.springframework.data.annotation.Id;
import org.springframework.data.mongodb.core.mapping.Document;

@Document(collection = "expense_category")
public class EquipmentCategory {
    @Id
    private String equipmentCategoryId;
    private String name;

    public EquipmentCategory() {
    }

    public EquipmentCategory(String equipmentCategoryId) {
        this.equipmentCategoryId = equipmentCategoryId;
    }

    public EquipmentCategory(String equipmentCategoryId, String name) {
        this.equipmentCategoryId = equipmentCategoryId;
        this.name = name;
    }

    public String getEquipmentCategoryId() {
        return equipmentCategoryId;
    }

    public void setEquipmentCategoryId(String equipmentCategoryId) {
        this.equipmentCategoryId = equipmentCategoryId;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

}