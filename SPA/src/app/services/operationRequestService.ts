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


}