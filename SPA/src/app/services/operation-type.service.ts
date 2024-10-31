import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
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

  constructor(private http: HttpClient) {}

  getOperationTypes(): Observable<OperationType[]> {
    return this.http.get<OperationType[]>(this.apiUrl);
  }
}