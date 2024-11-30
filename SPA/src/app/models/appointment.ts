export interface Appointment {
    
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
    startTime : string;
    endTime : string;
  }
  
  
  
 