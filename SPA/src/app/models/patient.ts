export interface Patient {
    medicalRecordNumber: string;
    name: string;
    dateOfBirth: Date;
    email: string;
    phoneNumber: string;
    gender: string;
    address: Address;
    emergencyContact: EmergencyContact;
    medicalHistory: MedicalHistory;
  }
  
  export interface Address {
    street: string;
    postalCode: string;
    city: string;
    country: string;
  }
  
  export interface EmergencyContact {
    name: string;
    email: string;
    phoneNumber: string
  }
  
  export interface MedicalHistory {
    medicalConditions: string;
    // appointments: Appointment[];
  }
  
  export interface PatientsView {
    medicalRecordNumber: string;
    name: string;
    dateOfBirth: Date;
    email: string
  }