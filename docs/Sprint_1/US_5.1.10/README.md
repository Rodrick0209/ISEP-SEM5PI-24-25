# US 5.1.10 - As an Admin, I want to delete a patient profile

## 1. Context

This US creates the functionality to delete a patient profile.

## 2. Requirements

**5.1.10** As an Admin, I want to delete a patient profile, so that I can remove patients who are no longer under care.

### 2.1. Acceptance Criteria

- Admins can search for a patient profile and mark it for deletion.
- Before deletion, the system prompts the admin to confirm the action.
- Once deleted, all patient data is permanently removed from the system within a predefined time frame.
- The system logs the deletion for audit and GDPR compliance purposes.

### 2.2. User Story Dependencies

**US 5.1.8** - The patient profile must be created on the system
**US 5.1.11** - The admin must select the patient by a type of data
