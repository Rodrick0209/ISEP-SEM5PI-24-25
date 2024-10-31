import { Injectable } from '@angular/core';
import { HttpClient, HttpParams } from '@angular/common/http';
import { Observable } from 'rxjs';

export interface RequiredStaff {
  num: number;
  specialization: string;
}

export interface Phase {
  duration: number;
  requiredStaff: RequiredStaff[];
}

export interface OperationType {
  name: string;
  status: boolean;
  specialization: string;
  preparationPhase: Phase;
  surgeryPhase: Phase;
  cleaningPhase: Phase;
}

@Injectable({
  providedIn: 'root'
})
export class OperationTypesService {
  private apiUrl = '/api/OperationType/GetAll'; // Update with your API URL
  private filterApiUrl = '/api/OperationType/Filter'; // Update with your filter API URL

  constructor(private http: HttpClient) {}

  getOperationTypes(): Observable<OperationType[]> {
    return this.http.get<OperationType[]>(this.apiUrl);
  }

  filterOperationTypes(name: string, status: string, specialization: string): Observable<OperationType[]> {
    let params = new HttpParams();
    if (name) {
      params = params.set('name', name);
    }
    if (status) {
      params = params.set('status', status);
    }
    if (specialization) {
      params = params.set('specialization', specialization);
    }
    return this.http.get<OperationType[]>(this.filterApiUrl, { params });
  }
}