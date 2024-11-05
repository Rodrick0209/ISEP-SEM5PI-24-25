import { HttpClient, HttpParams } from '@angular/common/http';
import { Injectable } from '@angular/core'
import { Observable } from 'rxjs';
import { AuthService } from './auth.service';

export interface Patient {
  medicalRecordNumber: string;
  name: string;
  dateOfBirth: Date;
  email: string;
  phoneNumber: string;
  gender: string;
  address: Address;
  emergencyContact: EmergencyContact;
  medicalHistory: MedicalHistory;
}

export interface Address {
  street: string;
  postalCode: string;
  city: string;
  country: string;
}

export interface EmergencyContact {
  name: string;
  email: string;
  phoneNumber: string
}

export interface MedicalHistory {
  medicalConditions: string;
  // appointments: Appointment[];
}

export interface PatientsView {
  medicalRecordNumber: string;
  name: string;
  dateOfBirth: Date;
  email: string
}

@Injectable({
  providedIn: 'root'
})
export class PatientService {
  private url = '/api/patients';
  private urlMedicalRecordNumber = '/api/patients/MedicalRecordNumber';
  private searchUrl = '/api/patients/search';

  constructor(private http: HttpClient, private authService: AuthService) { }

  getPatients(): Observable<PatientsView[]> {
    const headers = { 'Authorization': 'Bearer ' + this.authService.getToken() };
    console.log(headers)
    return this.http.get<PatientsView[]>(this.url, { headers });
  }

  getPatientByMedicalRecordNumber(medicalRecordNumber: string): Observable<Patient> {
    const headers = { 'Authorization': 'Bearer ' + this.authService.getToken() };
    return this.http.get<Patient>(`${this.urlMedicalRecordNumber}/${medicalRecordNumber}`, { headers });
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

    const headers = { 'Authorization': 'Bearer ' + this.authService.getToken() };
    return this.http.post(this.url, body, { headers });
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

    const headers = { 'Authorization': 'Bearer ' + this.authService.getToken() };
    return this.http.patch(`${this.url}/${medicalRecordNumber}`, body, { headers });
  }

  deletePatient(medicalRecordNumber: string) : Observable<any> {
    const headers = { 'Authorization': 'Bearer ' + this.authService.getToken() };
    return this.http.delete(`${this.url}/${medicalRecordNumber}`, { headers });
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

    const headers = { 'Authorization': 'Bearer ' + this.authService.getToken() };
    return this.http.get<PatientsView[]>(this.searchUrl, { params, headers });
  }
}
