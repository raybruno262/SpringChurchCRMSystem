package com.SpringChurchCRMSystem.SpringChurchCRMSystem.controller;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.SpringChurchCRMSystem.SpringChurchCRMSystem.model.Department;
import com.SpringChurchCRMSystem.SpringChurchCRMSystem.service.DepartmentService;

@RestController
@RequestMapping("/api/department")
public class DepartmentController {
    @Autowired
    private DepartmentService departmentService;

    // save department
    @PostMapping("/createDepartment")
    public ResponseEntity<String> createDepartment(@RequestBody Department department) {

        return departmentService.createDepartment(department);

    }

    // update department
    @PutMapping("/updateDepartment/{departmentId}")
    public ResponseEntity<String> updateDepartment(@PathVariable String departmentId,
            @RequestBody Department newDepartment) {

        return departmentService.updateDapartment(departmentId, newDepartment);

    }

    // delete department
    @DeleteMapping("/deleteDepartment/{departmentId}")
    public ResponseEntity<String> deleteDepartment(@PathVariable String departmentId) {

        return departmentService.deleteDepartment(departmentId);

    }

    // get all departments
    @GetMapping("/allDepartments")
    public List<Department> allDepartments() {

        return departmentService.getAllDepartments();

    }

    // get paginated departments
    @GetMapping("/getPaginatedDepartments")
    public Page<Department> getPaginatedDepartments(@RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "5") int size) {

        return departmentService.getPaginatedDepartments(page, size);

    }

}
