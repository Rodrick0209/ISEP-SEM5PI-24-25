export interface Appointment {
    
    id: string;
    appointmentTimeSlot: AppointmentTimeSlot; 
    appointmentStatus: AppointmentStatus; 
    operationRoomId: string; 
    operationRequestId: string; 
  }
  
  // Enum para a categoria (Category)
  export enum AppointmentStatus {
    Scheduled = "Scheduled",    
    Completed =  "Completed",  
    Cancelled  = "Cancelled"
    
  }
  
  
  export interface AppointmentTimeSlot {
    date : string;
    timeSlot : TimeSlot;
    
  }

  export interface TimeSlot {
    startTime : string;
    endTime : string;
  }


  export interface AppointmentEdit {
    id: string;
    appointmentTimeSlot: AppointmentTimeSlot;
    AppointmentStatus: AppointmentStatus;
    operationRoomId: string;
    operationRequestId: string;
    anesthesiaStaff : string[];
    surgeryStaff : string[];
    operationRequestTeamForCleaning : string[];
  }


  export interface AppointmentsView {

    id: string;
    operationRequestPriority : string;
    operationRequestPatientId : string;
    appointmentTimeSlot: AppointmentTimeSlot; 
    appointmentStatus: AppointmentStatus;
    operationRoomId : string;

  }

  export interface AppointmentTable {
    priority: string,
    doctor: string,
    appointmentTimeSlot: AppointmentTimeSlot,
    roomNumber: string,
  }


  export interface StaffPhase {
    specializationId: string;
    nrNeededStaff: number;
    staffId: string[];
  }
  
  export interface MedicalTeamShowForAppointmentCreate {
    staffAnesthesyPhase: StaffPhase[];
    staffSurgeryPhase: StaffPhase[];
  }
  
  
  
 