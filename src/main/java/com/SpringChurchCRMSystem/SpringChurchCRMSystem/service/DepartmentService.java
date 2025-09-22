package com.SpringChurchCRMSystem.SpringChurchCRMSystem.service;

import java.util.List;
import java.util.Optional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;

import com.SpringChurchCRMSystem.SpringChurchCRMSystem.model.Department;
import com.SpringChurchCRMSystem.SpringChurchCRMSystem.repository.DepartmentRepository;

@Service
public class DepartmentService {
    @Autowired
    private DepartmentRepository departmentRepository;

    // create new department
    public String createDepartment(Department department) {
        if (departmentRepository.findByName(department.getName()).isPresent()) {
            return "Department already exists";
        } else {
            departmentRepository.save(department);
            return "Department Saved Successfully";
        }
    }

    // update department
    public String updateDapartment(String departmentId, Department newDepartment) {
        Optional<Department> optdep = departmentRepository.findById(departmentId);
        if (optdep.isEmpty()) {
            return "Department not found";
        }
        Department dep = optdep.get();
        dep.setDepartmentId(departmentId);
        dep.setName(newDepartment.getName());
        departmentRepository.save(dep);
        return "Department updated successfully";

    }

    // delete department
    public String deleteDepartment(String departmentId) {
        Optional<Department> optdep = departmentRepository.findById(departmentId);
        if (optdep.isEmpty()) {
            return "Department not found";
        }
        departmentRepository.deleteById(departmentId);
        return "Department Deleted Successfully";

    }

    // get all departments
    public List<Department> getAllDepartments() {
        return departmentRepository.findAll();

    }

    // get all paginated departments
    public Page<Department> getPaginatedDepartments(int page, int size) {
        PageRequest pageable = PageRequest.of(page, size);
        return departmentRepository.findAll(pageable);

    }

}
