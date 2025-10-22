package com.SpringChurchCRMSystem.SpringChurchCRMSystem.model;

import java.time.LocalDate;

import org.springframework.data.annotation.Id;
import org.springframework.data.mongodb.core.mapping.DBRef;
import org.springframework.data.mongodb.core.mapping.Document;

@Document(collection = "equipment")
public class Equipment {
    @Id
    private String equipmentId;
    private String name;
    @DBRef
    private EquipmentCategory equipmentCategory;
    private LocalDate purchaseDate;
    private double purchasePrice;
    private String condition; // Excellent, good, needs repair, Out of Service
    private String location; // where it is going to be used
    private String description;

    @DBRef
    private Level level;

    public Equipment() {
    }

    public Equipment(String equipmentId) {
        this.equipmentId = equipmentId;
    }

    public Equipment(String equipmentId, String name, EquipmentCategory equipmentCategory, LocalDate purchaseDate,
            double purchasePrice, String condition, String location, String description, Level level) {
        this.equipmentId = equipmentId;
        this.name = name;
        this.equipmentCategory = equipmentCategory;
        this.purchaseDate = purchaseDate;
        this.purchasePrice = purchasePrice;
        this.condition = condition;
        this.location = location;
        this.description = description;
        this.level = level;
    }

    public String getEquipmentId() {
        return equipmentId;
    }

    public void setEquipmentId(String equipmentId) {
        this.equipmentId = equipmentId;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public EquipmentCategory getEquipmentCategory() {
        return equipmentCategory;
    }

    public void setEquipmentCategory(EquipmentCategory equipmentCategory) {
        this.equipmentCategory = equipmentCategory;
    }

    public LocalDate getPurchaseDate() {
        return purchaseDate;
    }

    public void setPurchaseDate(LocalDate purchaseDate) {
        this.purchaseDate = purchaseDate;
    }

    public double getPurchasePrice() {
        return purchasePrice;
    }

    public void setPurchasePrice(double purchasePrice) {
        this.purchasePrice = purchasePrice;
    }

    public String getCondition() {
        return condition;
    }

    public void setCondition(String condition) {
        this.condition = condition;
    }

    public String getLocation() {
        return location;
    }

    public void setLocation(String location) {
        this.location = location;
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
