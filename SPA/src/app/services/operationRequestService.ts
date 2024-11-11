import { HttpClient, HttpParams } from '@angular/common/http';
import { Injectable } from '@angular/core'
import { Observable } from 'rxjs';
import { AuthService } from './auth.service';


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



@Injectable({
    providedIn: 'root'
})
export class OperationRequestService {
    private url = '/api/OperationRequest/GetAllForUi';
    
    constructor(private http: HttpClient, private authService: AuthService) { }

    getOperationRequests(): Observable<OperationRequest[]> {
        return this.http.get<OperationRequest[]>(this.url);
    }

    deleteOperationRequest(id: string): Observable<any> {
        return this.http.delete(`/api/OperationRequest/${id}`);
    }


    editOperationRequest(data: any): Observable<any> {
        console.log(data);
        return this.http.put(`/api/OperationRequest/${data.Id}`,data);
    }


    filterOperationRequests(filter: { operationType: string; patientName: string, medicalRecordNumber: string, startDate: string, endDate: string}): Observable<OperationRequest[]> {
        let params = new HttpParams();
        if (filter.operationType) {
            params = params.set('OperationType', filter.operationType);
        }
        if (filter.patientName) {
            params = params.set('PatientName', filter.patientName);
        }
        if (filter.medicalRecordNumber) {
            params = params.set('MedicalRecordNumber', filter.medicalRecordNumber);
        }
        if (filter.startDate) {
            params = params.set('StartDate', filter.startDate);
        }
        if (filter.endDate) {
            params = params.set('EndDate', filter.endDate);
        }
        console.debug('Filtering Request Entered');
        return this.http.get<OperationRequest[]>('/api/OperationRequest/getWithFilters', { params });
    }





}