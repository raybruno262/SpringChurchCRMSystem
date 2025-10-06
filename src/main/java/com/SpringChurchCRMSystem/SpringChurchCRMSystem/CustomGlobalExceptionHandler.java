package com.SpringChurchCRMSystem.SpringChurchCRMSystem;

import org.springframework.http.HttpStatus;
import org.springframework.http.converter.HttpMessageNotReadableException;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.web.bind.MethodArgumentNotValidException;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.servlet.resource.NoResourceFoundException;
import org.springframework.web.server.ResponseStatusException;
import org.springframework.http.ResponseEntity;

@RestControllerAdvice
public class CustomGlobalExceptionHandler {
    // Blocked by inactive ancestor for enabling a level from inactive to active
    public static final String BLOCKED_BY_INACTIVE_ANCESTOR = "Status 8000";

    @ExceptionHandler(MethodArgumentNotValidException.class)
    public ResponseEntity<String> handleValidation(MethodArgumentNotValidException ex) {
        return ResponseEntity.ok("Status 3000"); // Bad request
    }

    @ExceptionHandler(HttpMessageNotReadableException.class)
    public ResponseEntity<String> handleMalformedJson(HttpMessageNotReadableException ex) {
        return ResponseEntity.ok("Status 3000"); // Bad request
    }

    @ExceptionHandler(ResponseStatusException.class)
    public ResponseEntity<String> handleResponseStatus(ResponseStatusException ex) {
        HttpStatus status = (HttpStatus) ex.getStatusCode();
        return ResponseEntity.ok(mapToCustomCode(status));
    }

    @ExceptionHandler(AccessDeniedException.class)
    public ResponseEntity<String> handleForbidden(AccessDeniedException ex) {
        return ResponseEntity.ok("Status 6000"); // Forbidden
    }

    @ExceptionHandler(NoResourceFoundException.class)
    public ResponseEntity<String> handleStaticResourceNotFound(NoResourceFoundException ex) {
        return ResponseEntity.ok("Status 7000"); // Route or static resource not found
    }

    @ExceptionHandler(Exception.class)
    public ResponseEntity<String> handleGeneric(Exception ex) {
        ex.printStackTrace();
        return ResponseEntity.ok("Status 9999"); // Unknown error
    }

    private String mapToCustomCode(HttpStatus status) {
        return switch (status) {
            case OK -> "Status 1000"; // 200 ok
            case INTERNAL_SERVER_ERROR -> "Status 2000"; // 500
            case BAD_REQUEST -> "Status 3000"; // 400
            case UNAUTHORIZED -> "Status 4000"; // 401
            case CONFLICT -> "Status 5000"; // 409
            case FORBIDDEN -> "Status 6000"; // 403
            case NOT_FOUND -> "Status 7000"; // 404

            default -> "Status 9999";
        };
    }
}