import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { Observable } from 'rxjs';
import { map } from 'rxjs/operators';
import { MedicalCondition } from '../models/medicalCondition';
import { MedicalConditionCatalog } from '../models/medicalConditionCatalog';

@Injectable({
  providedIn: 'root'
})
export class MedicalCondtionService {
  private baseUrl = '/api2/medicalConditions';
  private getAllergiesUrl = '/getAll';
  private createUrl = '/create';


    constructor(private http: HttpClient) {}

    getMedicalConditions(): Observable<MedicalConditionCatalog[]> {
        const url = `${this.baseUrl}${this.getAllergiesUrl}`;
        return this.http.get<MedicalConditionCatalog[]>(url).pipe(
            map((data: MedicalConditionCatalog[]) => data)
        );
    }

    createMedicalCondition(code: string, designation: string, description: string, commonSymptoms: string[]): Observable<MedicalConditionCatalog> {
        const url = `${this.baseUrl}${this.createUrl}`;
        const body: any = {};
        body.code = code;
        body.designation = designation;
        if(description) {
            body.description = description;
        }
        if(commonSymptoms) {
            body.commonSymptoms = commonSymptoms;
        }
        return this.http.post<MedicalConditionCatalog>(url, body).pipe(
            map((data: MedicalConditionCatalog) => data)
        );
    }

    getMedicalConditionCatalogItem(code: string): Observable<MedicalConditionCatalog> {
        const url = `${this.baseUrl}/get/${code}`;
        return this.http.get<MedicalConditionCatalog>(url).pipe(
            map((data: MedicalConditionCatalog) => data)
        );
    }

    updateMedicalConditionCatalogItem(code: string, designation: string, description: string): Observable<MedicalConditionCatalog> {
        const url = `${this.baseUrl}/update/${code}`;
        const body: any = {};
        if(designation) {
            body.designation = designation;
        }
        if(description) {
            body.description = description
        }
        return this.http.put<MedicalConditionCatalog>(url, body).pipe(
            map((data: MedicalConditionCatalog) => data)
        );
    }
}
