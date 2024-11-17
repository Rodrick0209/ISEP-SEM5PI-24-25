export interface Staff {
    id: string; // StaffId
    fullName: string; // FullName
    licenseNumber: string; // LicenseNumber
    specialization: string; // SpecializationId
    email: string; // Email
    phoneNumber: string; // PhoneNumber
    category: Category; // Enum correspondente
    status: StaffStatus; // Enum correspondente
  }
  
  // Enum para a categoria (Category)
  export enum Category {
    Doctor = "Doctor",
    InternDoctor = "Intern Doctor",
    Nurse = "Nurse",
    Technician = "Technician"
    
  }
  
  // Enum para o status do staff (StaffStatus)
  export enum StaffStatus {
    Active = "Active",
    Inactive = "Inactive"
  }
  
  
  
  export interface StaffsView {
    id: string;
    name: string;
    licenseNumber: string;
    phoneNumber: string;
    email: string;
    specialization: string
  }