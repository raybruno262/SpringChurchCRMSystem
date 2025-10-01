package com.SpringChurchCRMSystem.SpringChurchCRMSystem.model;

import org.springframework.data.mongodb.core.mapping.DBRef;

public class BaptismInformation {
    private boolean baptized; // true or false
    private boolean sameReligion; // true or false
    @DBRef
    private Level baptismCell; // only if sameReligion == true
    private String otherChurchName; // only is sameReligion == false
    private String otherChurchAddress; // only is sameReligion == false

    public BaptismInformation() {
    }

    public boolean isBaptized() {
        return baptized;
    }

    public void setBaptized(boolean baptized) {
        this.baptized = baptized;
    }

    public boolean isSameReligion() {
        return sameReligion;
    }

    public void setSameReligion(boolean sameReligion) {
        this.sameReligion = sameReligion;
    }

    public Level getBaptismCell() {
        return baptismCell;
    }

    public void setBaptismCell(Level baptismCell) {
        this.baptismCell = baptismCell;
    }

    public String getOtherChurchName() {
        return otherChurchName;
    }

    public void setOtherChurchName(String otherChurchName) {
        this.otherChurchName = otherChurchName;
    }

    public String getOtherChurchAddress() {
        return otherChurchAddress;
    }

    public void setOtherChurchAddress(String otherChurchAddress) {
        this.otherChurchAddress = otherChurchAddress;
    }

}
