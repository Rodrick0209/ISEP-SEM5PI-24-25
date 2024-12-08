import { HttpClient, HttpParams } from '@angular/common/http';
import { Injectable } from '@angular/core'
import { Observable } from 'rxjs';
import { AllergiesView, MedicalConditionView, MedicalRecord, Patient, PatientsView } from '../models/patient';
import { MedicalCondition } from '../models/medicalCondition';


@Injectable({
  providedIn: 'root'
})
export class PatientService {
  private url = '/api/patients';
  private urlMedicalRecordNumber = '/api/patients/MedicalRecordNumber';
  private searchUrl = '/api/patients/search';
  private getAllAllergiesUrl = '/api2/allergiesCatalog/getAll';
  private getAllMedicalCondsUrl = '/api2/medicalConditions/getAll';
  private editRecordUrl ='/api2/medicalRecord/update';
  private recordUrl = '/api2/medicalRecord';

  constructor(private http: HttpClient) { }

  getPatients(): Observable<PatientsView[]> {
    return this.http.get<PatientsView[]>(this.url);
  }

  getAllAllergies(): Observable<AllergiesView[]>{
    return this.http.get<AllergiesView[]>(this.getAllAllergiesUrl);
  }

  getAllAllMedicalConditions(): Observable<MedicalConditionView[]>{
    return this.http.get<MedicalConditionView[]>(this.getAllMedicalCondsUrl);
  }

  filterMedicalRecordEntries(selectedMedicalRecordNumber: string | null, name: string): Observable<MedicalRecord> {
    return this.http.get<MedicalRecord>(`${this.recordUrl}/${selectedMedicalRecordNumber}/${name}`);
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

  editPatient(medicalRecordNumber: string, name: string, email: string, phoneNumber: string, street: string, postalCode: string, city: string, country: string, allergies: AllergiesView[]) : Observable<any> {
    const body: any = {};
    body.medicalRecordNumber = medicalRecordNumber;
    if (name) body.name = name;
    if (email) body.email = email;
    if (phoneNumber) body.phoneNumber = phoneNumber;
    if (street) body.street = street;
    if (postalCode) body.postalCode = postalCode;
    if (city) body.city = city;
    if (country) body.country = country;
    
    const bodyMedicalRecord: any = {};

    bodyMedicalRecord.allergies = [];
    if (allergies) bodyMedicalRecord.allergies = allergies.map(allergy => allergy.name);
    bodyMedicalRecord.medicalConditions = [];
    
    // Update the medical record in the api2
    this.http.put(`${this.editRecordUrl}/${medicalRecordNumber}`, bodyMedicalRecord).subscribe(result => {
      console.log(result);
    });

    return this.http.patch(`${this.url}/${medicalRecordNumber}`, body);
  }

  deletePatient(medicalRecordNumber: string) : Observable<any> {
    return this.http.delete(`${this.url}/${medicalRecordNumber}`);
  }

  getMedicalRecordByPatientId(patientId: string): Observable<MedicalRecord> {
    return this.http.get<MedicalRecord>(`/api2/medicalRecord/getByPatientId/${patientId}`);
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
