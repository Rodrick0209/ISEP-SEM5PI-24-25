import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { Observable } from 'rxjs';
import { map } from 'rxjs/operators';
import { MedicalCondition } from '../models/medicalCondition';

@Injectable({
  providedIn: 'root'
})
export class MedicalCondtionService {
  private baseUrl = '/api2/medicalConditions';
  private getAllergiesUrl = '/getAll';
  private createUrl = '/create';


    constructor(private http: HttpClient) {}

    getMedicalConditions(): Observable<MedicalCondition[]> {
        const url = `${this.baseUrl}${this.getAllergiesUrl}`;
        return this.http.get<MedicalCondition[]>(url).pipe(
            map((data: MedicalCondition[]) => data)
        );
    }


    createMedicalCondition(name: string): Observable<MedicalCondition> {
        const url = `${this.baseUrl}${this.createUrl}`;
        return this.http.post<MedicalCondition>(url, { name }).pipe(
            map((data: MedicalCondition) => data)
        );
    }
}
