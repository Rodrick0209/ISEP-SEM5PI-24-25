import { HttpClient, HttpParams } from '@angular/common/http';
import { Injectable } from '@angular/core'
import { Observable } from 'rxjs';
import { AllergiesView, MedicalConditionView, MedicalRecord, Patient, PatientsView } from '../models/patient';
import { MedicalCondition } from '../models/medicalCondition';
import { Allergy } from '../models/Allergy';


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
  private recordUrl = '/api2/medicalRecord/search/';
  private getRecordForPatientId='/api2/medicalRecord/getByPatientId/'

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

  filterMedicalRecordEntries(selectedMedicalRecordNumber: string, name: string | null): Observable<MedicalRecord> {
      if (!name) {
        return this.getMedicalRecordForPatientId(selectedMedicalRecordNumber);
      }
      return this.http.get<MedicalRecord>(`${this.recordUrl}${selectedMedicalRecordNumber}/${name}`);
    }


  getMedicalRecordForPatientId(patientId: string): Observable<MedicalRecord> {
    return this.http.get<MedicalRecord>(`${this.getRecordForPatientId}${patientId}`);
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

  editPatient(medicalRecordNumber: string, name: string, email: string, phoneNumber: string, street: string, postalCode: string, city: string, country: string) : Observable<any> {
    const body: any = {};
    body.medicalRecordNumber = medicalRecordNumber;
    if (name) body.name = name;
    if (email) body.email = email;
    if (phoneNumber) body.phoneNumber = phoneNumber;
    if (street) body.street = street;
    if (postalCode) body.postalCode = postalCode;
    if (city) body.city = city;
    if (country) body.country = country;

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

  getPatientByEmail(email: string): Observable<Patient> {
    return this.http.get<Patient>(`${this.url}/email/${email}`);
  }


  updateMedicalRecord(medicalRecordNumber: string, allergies: Allergy[], medicalConditions: MedicalCondition[]): Observable<any> {
    const body = {
      allergies: allergies.map(allergy => ({
        name: allergy.name,
        description: allergy.description,
      })),
      medicalConditions: medicalConditions.map(condition => ({
        code: condition.code,
        designation: condition.designation,
        date: condition.date
      }))
    };

    

    return this.http.put(`${this.editRecordUrl}/${medicalRecordNumber}`, body);
  }
}
