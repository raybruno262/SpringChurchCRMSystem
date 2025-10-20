package com.SpringChurchCRMSystem.SpringChurchCRMSystem.service;

import java.util.List;
import java.util.Optional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import com.SpringChurchCRMSystem.SpringChurchCRMSystem.model.Department;
import com.SpringChurchCRMSystem.SpringChurchCRMSystem.repository.DepartmentRepository;

@Service
public class DepartmentService {
    @Autowired
    private DepartmentRepository departmentRepository;

    // create new department
    public ResponseEntity<String> createDepartment(Department department) {
        if (departmentRepository.findByNameIgnoreCase(department.getName()).isPresent()) {
            return ResponseEntity.ok("Status 5000"); // department exists
        } else {
            department.setName(department.getName().trim());

            departmentRepository.save(department);
            return ResponseEntity.ok("Status 1000"); // Success
        }
    }

    // update department
    public ResponseEntity<String> updateDapartment(String departmentId, Department newDepartment) {
        Optional<Department> optdep = departmentRepository.findById(departmentId);
        if (optdep.isEmpty()) {
            return ResponseEntity.ok("Status 3000");
        }
        Department dep = optdep.get();
        dep.setDepartmentId(departmentId);
        dep.setName(newDepartment.getName());
        departmentRepository.save(dep);
        return ResponseEntity.ok("Status 1000"); // Success

    }

    // get all departments
    public List<Department> getAllDepartments() {
        return departmentRepository.findAll(Sort.by(Sort.Direction.DESC, "departmentId"));

    }

    // deparment count
    public long getTotalDepartmentCount() {
        return departmentRepository.count();
    }

    // get all paginated departments
    public Page<Department> getPaginatedDepartments(int page, int size) {
        PageRequest pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.DESC, "departmentId"));
        return departmentRepository.findAll(pageable);

    }

}
