import { ComponentFixture, TestBed } from '@angular/core/testing';
import { ListPatientsComponent } from './list-patients.component';
import { PatientService } from '../../services/patient.service';
import { MessageService } from '../../services/message.service';
import { Router } from '@angular/router';
import { of, throwError } from 'rxjs';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { CommonModule } from '@angular/common';
import { FilterPatientsComponent } from '../filter-patients/filter-patients.component';



describe('ListPatientsComponent', () => {
  let component: ListPatientsComponent;
  let fixture: ComponentFixture<ListPatientsComponent>;
  let patientService: jasmine.SpyObj<PatientService>;
  let messageService: jasmine.SpyObj<MessageService>;
  let router: jasmine.SpyObj<Router>;

  beforeEach(async () => {
    const patientServiceSpy = jasmine.createSpyObj('PatientService', ['getPatients', 'filterPatients']);
    const messageServiceSpy = jasmine.createSpyObj('MessageService', ['getMessage']);
    const routerSpy = jasmine.createSpyObj('Router', ['navigate']);

    await TestBed.configureTestingModule({
      imports: [ListPatientsComponent, CommonModule, FilterPatientsComponent, HttpClientTestingModule],
      providers: [
        { provide: PatientService, useValue: patientServiceSpy },
        { provide: MessageService, useValue: messageServiceSpy },
        { provide: Router, useValue: routerSpy }
      ]
    }).compileComponents();

    fixture = TestBed.createComponent(ListPatientsComponent);
    component = fixture.componentInstance;
    patientService = TestBed.inject(PatientService) as jasmine.SpyObj<PatientService>;
    messageService = TestBed.inject(MessageService) as jasmine.SpyObj<MessageService>;
    router = TestBed.inject(Router) as jasmine.SpyObj<Router>;
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should filter patients', () => {
    const filter = { medicalRecordNumber: '123', name: 'John Doe', email: 'john@example.com', dateOfBirth: '1990-01-01' };
    const filteredPatients = [{ medicalRecordNumber: '123', name: 'John Doe', email: 'john@example.com', dateOfBirth: new Date('1990-01-01') }];
    patientService.filterPatients.and.returnValue(of(filteredPatients));

    component.onFilterChanged(filter);

    expect(component.filteredPatients).toEqual(filteredPatients);
    expect(component.totalPages).toBe(1);
    expect(component.currentPage).toBe(1);
  });

  it('should handle error when filtering patients', () => {
    const filter = { medicalRecordNumber: '123', name: 'John Doe', email: 'john@example.com', dateOfBirth: '1990-01-01' };
    patientService.filterPatients.and.returnValue(throwError('Error filtering patients'));

    component.onFilterChanged(filter);

    expect(component.filteredPatients).toEqual([]);
  });

  it('should navigate to create patient page', () => {
    component.createPatient();
    expect(router.navigate).toHaveBeenCalledWith(['/patient/create']);
  });

  it('should navigate to patient details page', () => {
    const patient = { medicalRecordNumber: '123', name: 'John Doe', email: 'john@example.com', dateOfBirth: new Date('1990-01-01') };
    component.seeDetails(patient);
    expect(router.navigate).toHaveBeenCalledWith(['/patient/details', '123']);
  });

  it('should navigate to edit patient page', () => {
    const patient = { medicalRecordNumber: '123', name: 'John Doe', email: 'john@example.com', dateOfBirth: new Date('1990-01-01') };
    component.editPatient(patient);
    expect(router.navigate).toHaveBeenCalledWith(['/patient/edit', '123']);
  });

  it('should navigate to delete patient page', () => {
    const patient = { medicalRecordNumber: '123', name: 'John Doe', email: 'john@example.com', dateOfBirth: new Date('1990-01-01') };
    component.deletePatient(patient);
    expect(router.navigate).toHaveBeenCalledWith(['/patient/delete', '123']);
  });

  it('should update pagination', () => {
    component.filteredPatients = Array.from({ length: 25 }, (_, i) => ({ medicalRecordNumber: `${i}`, name: `Patient ${i}`, email: `patient${i}@example.com`, dateOfBirth: new Date('1990-01-01') }));
    component.itemsPerPage = 10;
    component.currentPage = 2;

    component.updatePagination();

    expect(component.paginatedPatients.length).toBe(10);
    expect(component.paginatedPatients[0].medicalRecordNumber).toBe('10');
  });

  it('should go to next page', () => {
    component.totalPages = 3;
    component.currentPage = 1;

    component.nextPage();

    expect(component.currentPage).toBe(2);
  });

  it('should not go to next page if on last page', () => {
    component.totalPages = 3;
    component.currentPage = 3;

    component.nextPage();

    expect(component.currentPage).toBe(3);
  });

  it('should go to previous page', () => {
    component.currentPage = 2;

    component.previousPage();

    expect(component.currentPage).toBe(1);
  });

  it('should not go to previous page if on first page', () => {
    component.currentPage = 1;

    component.previousPage();

    expect(component.currentPage).toBe(1);
  });
});
