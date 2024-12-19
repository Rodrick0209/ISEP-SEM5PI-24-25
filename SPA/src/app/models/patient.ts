export interface Patient {
    id: string;
    medicalRecordNumber: string;
    name: string;
    dateOfBirth: Date;
    email: string;
    phoneNumber: string;
    gender: string;
    address: Address;
    emergencyContact: EmergencyContact;
    medicalRecord: MedicalRecord;
  }

  export interface MedicalRecord {
    id: string;
    patientId: string;
    allergies : Allergy[];
    medicalConditions : MedicalCondition[];
  }

  export interface Allergy{
    id: string;
    code: string,
    designation: string,
    description: string,
  }

  export interface MedicalCondition{
    id: string;
    code: string,
    designation: string,
    date: Date
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
  
  export interface PatientsView {
    medicalRecordNumber: string;
    name: string;
    dateOfBirth: Date;
    email: string
  }

  export interface AllergiesView {
    code: string;
    designation: string
  }

  export interface MedicalConditionView {
    code: string;
    designation: string
  }