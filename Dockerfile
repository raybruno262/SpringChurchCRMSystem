# Stage 1: Build the Spring Boot app using JDK 23
FROM maven:3.9.4-eclipse-temurin-23 AS builder
WORKDIR /build
COPY . .
RUN mvn clean package -DskipTests

# Stage 2: Run the app with JDK 23
FROM eclipse-temurin:23-jdk
WORKDIR /app
COPY --from=builder /build/target/*.jar app.jar

# Run the Spring Boot application
ENTRYPOINT ["java", "-jar", "app.jar"]
