package com.SpringChurchCRMSystem.SpringChurchCRMSystem.dto;

import com.SpringChurchCRMSystem.SpringChurchCRMSystem.model.Level;
import com.SpringChurchCRMSystem.SpringChurchCRMSystem.model.LevelType;

public class LevelDTO {
    private String levelId;
    private String name;
    private String address;
    private LevelType levelType;

    public LevelDTO(Level level) {
        this.levelId = level.getLevelId();
        this.name = level.getName();
        this.address = level.getAddress();
        this.levelType = level.getLevelType();
    }

    public String getLevelId() {
        return levelId;
    }

    public void setLevelId(String levelId) {
        this.levelId = levelId;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getAddress() {
        return address;
    }

    public void setAddress(String address) {
        this.address = address;
    }

    public LevelType getLevelType() {
        return levelType;
    }

    public void setLevelType(LevelType levelType) {
        this.levelType = levelType;
    }

}
