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
    medicalHistory: MedicalHistory;
    medicalRecord: MedicalRecord;
  }

  export interface MedicalRecord {
    id: string;
    patientId: string;
    allergies : Allergy[];
    medicalConditions : MedicalCondition[];
  }

  export interface Allergy{
    name: string;
    //date: Date;
    //description:string;
  }

  export interface MedicalCondition{
    name: string;
    date: Date;
    description:string;
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

  export interface AllergiesView {
    name: string;
    
  }

  export interface MedicalConditionView {
    name: string;
  }