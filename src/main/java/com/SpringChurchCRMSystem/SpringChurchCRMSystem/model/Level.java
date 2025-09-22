package com.SpringChurchCRMSystem.SpringChurchCRMSystem.model;

import org.springframework.data.annotation.Id;
import org.springframework.data.mongodb.core.mapping.DBRef;
import org.springframework.data.mongodb.core.mapping.Document;

@Document(collection = "level")
public class Level {
    @Id
    private String levelId;
    private String name;
    private String address;
    private LevelType levelType;

    @DBRef
    private Level parent;

    public Level() {
    }

    public Level(String levelId) {
        this.levelId = levelId;
    }

    public Level(String levelId, String name, String address, LevelType levelType, Level parent) {
        this.levelId = levelId;
        this.name = name;
        this.address = address;
        this.levelType = levelType;
        this.parent = parent;
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

    public Level getParent() {
        return parent;
    }

    public void setParent(Level parent) {
        this.parent = parent;
    }

}
