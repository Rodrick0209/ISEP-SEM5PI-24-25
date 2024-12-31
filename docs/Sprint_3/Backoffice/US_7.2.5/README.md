# US 7.2.5 - As a Doctor, I want to search for Medical Conditions, so that I can use it to update the Patient Medical Record


## 1. Context

This User Story enables the functionality for searching medical conditions, allowing the doctor to use the information to update the patient's medical record.

---

## 2. Requirements

**7.2.5** As a Doctor, I want to search for Medical Conditions, so that I can use it to update the Patient Medical Record.

### 2.1 User Story Dependencies

This User Story has the following dependencies:
- Patient medical records must be accessible to the doctor.
- Medical condition data must be available in the system.
- The medical conditions must be structured in a searchable format to facilitate retrieval and updates.

---

## 3. Analysis

The doctor should be able to:
- Search for medical conditions by patient ID, name, or condition type.
- View detailed information about medical conditions, including diagnosis, severity, treatment, and relevant notes.
- Select and update the patient’s medical record with the medical condition data.
- Add new medical conditions to the record if necessary.
- Edit existing medical conditions to ensure the record is accurate and up to date.
- Ensure that duplicate conditions are prevented and that only relevant conditions are added.


---

## 5. Applied Patterns

- **GRASP Pattern:** Assigns responsibility for medical condition data search and updates to controller classes.
- **CRUD Pattern:** The system supports the basic operations for medical conditions—search, read, update, and delete.
- **SOLID Principles:**
  - **Single Responsibility Principle:** Medical condition management is handled by dedicated components.
  - **Open-Closed Principle:** The search and update system can be extended to support new medical condition-related features.
  - **Liskov Substitution Principle:** Ensures derived medical condition data types are compatible with the base functionality.
  - **Interface Segregation Principle:** Defines focused interfaces for medical condition-related operations.
  - **Dependency Inversion Principle:** High-level components interact with abstract interfaces rather than concrete classes.
- **DTO Pattern:** Used to transfer medical condition data without business logic.

---

## 6. Observations

- The search functionality should include filters and auto-complete options to assist doctors in quickly finding relevant medical conditions.
- The system should support adding new medical conditions if they do not already exist in the system.
- Alerts should be triggered for critical or severe medical conditions when added or updated in the patient's record.
- Medical condition data should be updated in real-time across the system to ensure that all medical staff have the latest information.