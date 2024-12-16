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
    appointmentStatus: AppointmentStatus;
    operationRoomId: string;
    operationRequestId: string;
    operationRequestTeamForAnesthesy : string[];
    operationRequestTeamForSurgery : string[];
  }


  export interface AppointmentsView {

    id: string;
    operationRequestPriority : string;
    operationRequestPatientId : string;
    appointmentTimeSlot: AppointmentTimeSlot; 
    appointmentStatus: AppointmentStatus;
    operationRoomNumber : string;

  }

  export interface AppointmentTable {
    priority: string,
    doctor: string,
    appointmentTimeSlot: AppointmentTimeSlot,
    roomNumber: string,
  }
  
  
  
 