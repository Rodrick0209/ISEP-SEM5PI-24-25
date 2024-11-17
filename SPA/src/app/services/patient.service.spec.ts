import { TestBed } from '@angular/core/testing';
import { HttpClientTestingModule, HttpTestingController } from '@angular/common/http/testing';
import { PatientService } from './patient.service';
import { Patient, PatientsView } from '../models/patient';

describe('PatientService', () => {
  let service: PatientService;
  let httpMock: HttpTestingController;

  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [HttpClientTestingModule],
      providers: [PatientService]
    });
    service = TestBed.inject(PatientService);
    httpMock = TestBed.inject(HttpTestingController);
  });

  afterEach(() => {
    httpMock.verify();
  });

  it('should be created', () => {
    expect(service).toBeTruthy();
  });

  it('should retrieve patients from the API via GET', () => {
    const dummyPatients: PatientsView[] = [
      { medicalRecordNumber: '1', name: 'John Doe', dateOfBirth: new Date('1990-01-01'), email: 'john.doe@example.com'},
      { medicalRecordNumber: '2', name: 'Jane Doe', dateOfBirth: new Date('1992-02-02'), email: 'jane.doe@example.com'}
    ];

    service.getPatients().subscribe(patients => {
      expect(patients.length).toBe(2);
      expect(patients).toEqual(dummyPatients);
    });

    const req = httpMock.expectOne(service['url']);
    expect(req.request.method).toBe('GET');
    req.flush(dummyPatients);
  });

  it('should retrieve a patient by medical record number from the API via GET', () => {
    const dummyPatient: Patient = {
      medicalRecordNumber: '12345',
      name: 'John Doe',
      dateOfBirth: new Date('1990-01-01'),
      email: 'john.doe@example.com',
      phoneNumber: '1234567890',
      gender: 'Male',
      address: {
        street: '123 Main St',
        postalCode: '12345',
        city: 'Anytown',
        country: 'USA'
      },
      emergencyContact: {
        name: 'Jane Doe',
        email: 'jane.doe@example.com',
        phoneNumber: '0987654321'
      },
      medicalHistory: {
        medicalConditions: 'None'
      }
    };

    service.getPatientByMedicalRecordNumber('12345').subscribe(patient => {
      expect(patient).toEqual(dummyPatient);
    });

    const req = httpMock.expectOne(`${service['urlMedicalRecordNumber']}/12345`);
    expect(req.request.method).toBe('GET');
    req.flush(dummyPatient);
  });

  it('should create a new patient via POST', () => {
    const newPatient = {
      firstName: 'John', lastName: 'Doe', fullName: 'John Doe', dateOfBirth: '1990-01-01', gender: 'Male', email: 'john.doe@example.com', phoneNumber: '1234567890',
      street: '123 Main St', postalCode: '12345', city: 'Anytown', country: 'USA',
      emergencyContactName: 'Jane Doe', emergencyContactEmail: 'jane.doe@example.com', emergencyContactPhoneNumber: '0987654321'
    };

    service.createPatient(
      newPatient.firstName, newPatient.lastName, newPatient.fullName, newPatient.dateOfBirth, newPatient.gender, newPatient.email, newPatient.phoneNumber,
      newPatient.street, newPatient.postalCode, newPatient.city, newPatient.country,
      newPatient.emergencyContactName, newPatient.emergencyContactEmail, newPatient.emergencyContactPhoneNumber
    ).subscribe(response => {
      expect(response).toEqual(newPatient);
    });

    const req = httpMock.expectOne(service['url']);
    expect(req.request.method).toBe('POST');
    expect(req.request.body).toEqual(newPatient);
    req.flush(newPatient);
  });

  it('should edit an existing patient via PATCH', () => {
    const updatedPatient = { medicalRecordNumber: '12345', name: 'John Doe', email: 'john.doe@example.com', phoneNumber: '1234567890', street: '123 Main St', postalCode: '12345', city: 'Anytown', country: 'USA', medicalConditions: 'None' };

    service.editPatient(
      updatedPatient.medicalRecordNumber, updatedPatient.name, updatedPatient.email, updatedPatient.phoneNumber, updatedPatient.street, updatedPatient.postalCode, updatedPatient.city, updatedPatient.country, updatedPatient.medicalConditions
    ).subscribe(response => {
      expect(response).toEqual(updatedPatient);
    });

    const req = httpMock.expectOne(`${service['url']}/12345`);
    expect(req.request.method).toBe('PATCH');
    expect(req.request.body).toEqual(updatedPatient);
    req.flush(updatedPatient);
  });

  it('should delete a patient via DELETE', () => {
    service.deletePatient('12345').subscribe(response => {
      expect(response).toBeNull();
    });

    const req = httpMock.expectOne(`${service['url']}/12345`);
    expect(req.request.method).toBe('DELETE');
    req.flush(null);
  });

  it('should filter patients via GET with query params', () => {
    const filteredPatients: PatientsView[] = [
      { medicalRecordNumber: '1', name: 'John Doe', dateOfBirth: new Date('1990-01-01'), email: 'john.doe@example.com'}
    ];

    service.filterPatients('12345', 'John Doe', '1990-01-01', 'john.doe@example.com').subscribe(patients => {
      expect(patients).toEqual(filteredPatients);
    });

    const req = httpMock.expectOne(req => req.url === service['searchUrl'] && req.params.has('medicalRecordNumber') && req.params.has('name') && req.params.has('dateOfBirth') && req.params.has('email'));
    expect(req.request.method).toBe('GET');
    req.flush(filteredPatients);
  });
});