# US 7.2.18 - As a Doctor, I want to update an allergy entry in a medical record

---

## 1. Context

This User Story enables the doctor to view and update allergy entries in a patient's medical record when accessing the record. The doctor should be able to see a table with the patient's allergies, update them, and remove any if necessary.

---

## 2. Requirements

**7.2.18** As a Doctor, I want to update an allergy entry in a medical record, so that I can ensure the patient's medical record remains accurate and up to date.

### 2.1 User Story Dependencies

This User Story depends on:
- The patient’s medical record being accessible in the system.
- Allergy data being linked to the patient’s medical record and retrievable in a structured format.
- The user interface for viewing and editing medical records must allow modification of allergy entries.

---

## 3. Analysis

When the doctor views a patient's medical record, the following features should be available:
- A table displaying all the allergies associated with the patient’s record.
- The doctor should be able to:
  - **Edit** allergy details (e.g., severity, description, reactions).
  - **Remove** any allergy entries if they are no longer relevant.
  - **Add new allergy entries** if needed.
  - Ensure that any updates are reflected in real-time across the system to keep the medical record accurate.

The table should have the following editable columns:
- Allergy Name
- Severity
- Date of Diagnosis
- Notes/Comments

---

## 4. Design - Process View


---

## 5. Applied Patterns

- **GRASP Pattern:** The responsibility of updating allergy entries is assigned to a specific controller or service that handles medical record data.
- **CRUD Pattern:** The system supports creating, reading, updating, and deleting allergy entries from the patient’s record.
- **SOLID Principles:**
  - **Single Responsibility Principle:** The allergy update functionality is separate from other medical record operations to maintain clear boundaries.
  - **Open-Closed Principle:** The allergy entry update system can be extended to support additional allergy-related features without modifying existing code.
  - **Liskov Substitution Principle:** Allergy data can be substituted by related data types, ensuring compatibility in the update process.
  - **Interface Segregation Principle:** The allergy management interface is focused and specific to allergy updates, avoiding unnecessary complexity.
  - **Dependency Inversion Principle:** High-level components interact with abstract services for allergy management, rather than concrete implementations.
- **DTO Pattern:** Data Transfer Objects (DTOs) are used to update allergy information in a structured format without business logic.

---

## 6. Observations

- The user interface for editing allergies should be intuitive and provide clear buttons or controls to add, edit, and delete allergy entries.
- The system should ensure that all allergy-related changes are saved in real-time, minimizing the risk of outdated or incorrect medical records.
- When allergies are updated, the system should check for any conflicting information, such as previously recorded reactions or treatments, to avoid discrepancies.
- The allergy entries table should have search and filter options to help doctors easily locate the relevant information.