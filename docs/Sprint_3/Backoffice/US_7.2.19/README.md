# US 7.2.19 - As a Doctor, I want to update a medical condition entry in a medical record

---

## 1. Context

This User Story enables the doctor to view and update medical condition entries in a patient's medical record. The doctor should be able to see a table with the patient's medical conditions, update them, and remove any conditions if necessary.

---

## 2. Requirements

**7.2.19** As a Doctor, I want to update a medical condition entry in a medical record, so that I can ensure the patient's medical record remains accurate and up to date.

### 2.1 User Story Dependencies

This User Story depends on:
- The patient’s medical record being accessible in the system.
- Medical condition data being linked to the patient’s medical record and retrievable in a structured format.
- The user interface for viewing and editing medical records must allow modification of medical condition entries.

---

## 3. Analysis

When the doctor views a patient's medical record, the following features should be available:
- A table displaying all the medical conditions associated with the patient’s record.
- The doctor should be able to:
  - **Edit** medical condition details (e.g., diagnosis, status, progression).
  - **Remove** any medical condition entries if they are no longer relevant.
  - **Add new medical condition entries** if needed.
  - Ensure that any updates are reflected in real-time across the system to keep the medical record accurate.

The table should have the following editable columns:
- Condition Name
- Diagnosis Date
- Status (Active/Inactive)
- Notes/Comments

---

## 4. Design - Process View

### Level 1
![Process View - Level 1](L1/MedicalConditionUpdate_L1view.svg)

### Level 2
![Process View - Level 2](L2/MedicalConditionUpdate_L2view.svg)

### Level 3
![Process View - Level 3](L3/MedicalConditionUpdate_L3view.svg)

---

## 5. Applied Patterns

- **GRASP Pattern:** The responsibility of updating medical condition entries is assigned to a specific controller or service that handles medical record data.
- **CRUD Pattern:** The system supports creating, reading, updating, and deleting medical condition entries from the patient’s record.
- **SOLID Principles:**
  - **Single Responsibility Principle:** The medical condition update functionality is separate from other medical record operations to maintain clear boundaries.
  - **Open-Closed Principle:** The medical condition entry update system can be extended to support additional condition-related features without modifying existing code.
  - **Liskov Substitution Principle:** Medical condition data can be substituted by related data types, ensuring compatibility in the update process.
  - **Interface Segregation Principle:** The medical condition management interface is focused and specific to condition updates, avoiding unnecessary complexity.
  - **Dependency Inversion Principle:** High-level components interact with abstract services for medical condition management, rather than concrete implementations.
- **DTO Pattern:** Data Transfer Objects (DTOs) are used to update medical condition information in a structured format without business logic.

---

## 6. Observations

- The user interface for editing medical conditions should be intuitive and provide clear buttons or controls to add, edit, and delete medical condition entries.
- The system should ensure that all medical condition-related changes are saved in real-time, minimizing the risk of outdated or incorrect medical records.
- When medical conditions are updated, the system should check for any conflicting information, such as previously recorded diagnoses or treatments, to avoid discrepancies.
- The medical condition entries table should have search and filter options to help doctors easily locate the relevant information.