package com.SpringChurchCRMSystem.SpringChurchCRMSystem.model;

import org.springframework.data.mongodb.core.mapping.DBRef;

public class BaptismInformation {
    private Boolean baptized; // true or false
    private Boolean sameReligion; // true or false
    @DBRef
    private Level baptismCell; // only if sameReligion == true
    private String otherChurchName; // only is sameReligion == false
    private String otherChurchAddress; // only is sameReligion == false

    public BaptismInformation() {
    }

    public BaptismInformation(Boolean baptized) {
        this.baptized = baptized;
    }

    public BaptismInformation(Boolean baptized, Boolean sameReligion, Level baptismCell, String otherChurchName,
            String otherChurchAddress) {
        this.baptized = baptized;
        this.sameReligion = sameReligion;
        this.baptismCell = baptismCell;
        this.otherChurchName = otherChurchName;
        this.otherChurchAddress = otherChurchAddress;
    }

    public Boolean getBaptized() {
        return baptized;
    }

    public void setBaptized(Boolean baptized) {
        this.baptized = baptized;
    }

    public Boolean getSameReligion() {
        return sameReligion;
    }

    public void setSameReligion(Boolean sameReligion) {
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
