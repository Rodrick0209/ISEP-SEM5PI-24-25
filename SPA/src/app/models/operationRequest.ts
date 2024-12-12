export interface OperationRequest {
    id: string;
    doctorThatWillPerformId: string;
    deadLineDate: Date;
    priority: string;
    patientId: string;
    operationTypeId: string;
}


export interface OperationRequestView {
    doctorThatWillPerformId: string;
    deadLineDate: Date;
    priority: string;
    patientId: string;
    operationTypeId: string;
}
  
 
  
  
  
 