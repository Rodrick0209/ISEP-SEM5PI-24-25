import { HttpClient, HttpParams } from '@angular/common/http';
import { Injectable } from '@angular/core'
import { Observable } from 'rxjs';
import { AuthService } from './auth.service';


export interface OperationRequest {
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



}