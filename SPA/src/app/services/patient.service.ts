import { HttpClient, HttpParams } from '@angular/common/http';
import { Injectable } from '@angular/core'
import { Observable } from 'rxjs';
import { Patient, PatientsView } from '../models/patient';


@Injectable({
  providedIn: 'root'
})
export class PatientService {
  private url = '/api/patients';
  private urlMedicalRecordNumber = '/api/patients/MedicalRecordNumber';
  private searchUrl = '/api/patients/search';

  constructor(private http: HttpClient) { }

  getPatients(): Observable<PatientsView[]> {
    return this.http.get<PatientsView[]>(this.url);
  }

  getPatientByMedicalRecordNumber(medicalRecordNumber: string): Observable<Patient> {
    return this.http.get<Patient>(`${this.urlMedicalRecordNumber}/${medicalRecordNumber}`);
  }

  createPatient(firstName: string, lastName: string, fullName: string, dateOfBirth: string, gender: string, email: string, phoneNumber: string,
    street: string, postalCode: string, city: string, country: string,
    emergencyContactName: string, emergencyContactEmail: string, emergencyContactPhoneNumber: string): Observable<any> {
    const body = {
      firstName: firstName,
      lastName: lastName,
      fullName: fullName,
      dateOfBirth: dateOfBirth,
      gender: gender,
      email: email,
      phoneNumber: phoneNumber,
      street: street,
      postalCode: postalCode,
      city: city,
      country: country,
      emergencyContactName: emergencyContactName,
      emergencyContactEmail: emergencyContactEmail,
      emergencyContactPhoneNumber: emergencyContactPhoneNumber
    }

    return this.http.post(this.url, body);
  }

  editPatient(medicalRecordNumber: string, name: string, email: string, phoneNumber: string, street: string, postalCode: string, city: string, country: string, medicalConditions: string) : Observable<any> {
    const body: any = {};
    body.medicalRecordNumber = medicalRecordNumber;
    if (name) body.name = name;
    if (email) body.email = email;
    if (phoneNumber) body.phoneNumber = phoneNumber;
    if (street) body.street = street;
    if (postalCode) body.postalCode = postalCode;
    if (city) body.city = city;
    if (country) body.country = country;
    if (medicalConditions) body.medicalConditions = medicalConditions;

    return this.http.patch(`${this.url}/${medicalRecordNumber}`, body);
  }

  deletePatient(medicalRecordNumber: string) : Observable<any> {
    return this.http.delete(`${this.url}/${medicalRecordNumber}`);
  }

  filterPatients(medicalRecordNumber: string, name: string, dateOfBirth: string, email: string): Observable<PatientsView[]> {
    let params = new HttpParams();
    if (medicalRecordNumber) {
      params = params.set('medicalRecordNumber', medicalRecordNumber);
    }
    if (name) {
      params = params.set('name', name);
    }
    if (dateOfBirth) {
      params = params.set('dateOfBirth', dateOfBirth);
    }
    if (email) {
      params = params.set('email', email);
    }

    return this.http.get<PatientsView[]>(this.searchUrl, { params });
  }
}
