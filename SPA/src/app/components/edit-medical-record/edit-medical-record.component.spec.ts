import { ComponentFixture, TestBed } from '@angular/core/testing';
import { Router } from '@angular/router';
import { of, throwError } from 'rxjs';
import { DatePipe } from '@angular/common';
import { EditMedicalRecordComponent } from './edit-medical-record.component';
import { PatientService } from '../../services/patient.service';
import { MessageService } from '../../services/message.service';
import { ActivatedRoute } from '@angular/router';
import { MedicalRecord, AllergiesView, MedicalConditionView } from '../../models/patient';

describe('EditMedicalRecordComponent', () => {
    let component: EditMedicalRecordComponent;
    let fixture: ComponentFixture<EditMedicalRecordComponent>;
    let patientService: jasmine.SpyObj<PatientService>;
    let router: jasmine.SpyObj<Router>;
    let messageService: jasmine.SpyObj<MessageService>;
    let route: ActivatedRoute;
    let datePipe: DatePipe;

    beforeEach(async () => {
        const patientServiceSpy = jasmine.createSpyObj('PatientService', ['getMedicalRecordByPatientId', 'updateMedicalRecord', 'getAllAllergies', 'getAllAllMedicalConditions']);
        const routerSpy = jasmine.createSpyObj('Router', ['navigate']);
        const messageServiceSpy = jasmine.createSpyObj('MessageService', ['getMessage']);

        await TestBed.configureTestingModule({
            providers: [
                { provide: PatientService, useValue: patientServiceSpy },
                { provide: Router, useValue: routerSpy },
                { provide: MessageService, useValue: messageServiceSpy },
                { provide: ActivatedRoute, useValue: { snapshot: { paramMap: { get: () => '123' } } } },
                DatePipe
            ]
        }).compileComponents();

        fixture = TestBed.createComponent(EditMedicalRecordComponent);
        component = fixture.componentInstance;
        patientService = TestBed.inject(PatientService) as jasmine.SpyObj<PatientService>;
        router = TestBed.inject(Router) as jasmine.SpyObj<Router>;
        messageService = TestBed.inject(MessageService) as jasmine.SpyObj<MessageService>;
        route = TestBed.inject(ActivatedRoute);
        datePipe = TestBed.inject(DatePipe);
    });

    it('should create', () => {
        expect(component).toBeTruthy();
    });

    it('should initialize with success message and medical record number', () => {
        messageService.getMessage.and.returnValue('Success');
        patientService.getAllAllergies.and.returnValue(of([]));
        patientService.getAllAllMedicalConditions.and.returnValue(of([]));
        patientService.getMedicalRecordByPatientId.and.returnValue(of({ id: '1', patientId: '2', allergies: [], medicalConditions: [] }));
        component.ngOnInit();
        expect(component.successMessage).toBe('Success');
        expect(component.medicalRecordNumber).toBe('123');
    });

    it('should get medical record on init', () => {
        const medicalRecord: MedicalRecord = {
            allergies: [], medicalConditions: [],
            id: '',
            patientId: ''
        };
        patientService.getAllAllergies.and.returnValue(of([]));
        patientService.getAllAllMedicalConditions.and.returnValue(of([]));
        patientService.getMedicalRecordByPatientId.and.returnValue(of(medicalRecord));
        component.ngOnInit();
        expect(patientService.getMedicalRecordByPatientId).toHaveBeenCalledWith('123');
        expect(component.medicalRecord).toEqual(medicalRecord);
    });

    it('should handle error when failing to get medical record', () => {
        patientService.getAllAllergies.and.returnValue(of([]));
        patientService.getAllAllMedicalConditions.and.returnValue(of([]));
        patientService.getMedicalRecordByPatientId.and.returnValue(throwError('error'));
        component.ngOnInit();
        expect(component.errorMessage).toBe('Failed to get medical record');
    });

    it('should navigate to edit medical record on update', () => {
        component.medicalRecordNumber = '123';
        component.updateMedicalRecord();
        expect(router.navigate).toHaveBeenCalledWith(['/edit-medical-record', '123']);
    });

    it('should add a new allergy', () => {
        component.medicalRecord = { id: '1', patientId: '2', allergies: [], medicalConditions: [] };
        component.addAllergy();
        expect(component.medicalRecord.allergies.length).toBe(1);
    });

    it('should add a new medical condition', () => {
        component.medicalRecord = { id: '1', patientId: '2', allergies: [], medicalConditions: [] };
        component.addMedicalCondition();
        expect(component.medicalRecord.medicalConditions.length).toBe(1);
    });

    it('should save medical record and navigate on success', () => {
        component.medicalRecord = { id: '1', patientId: '2',allergies: [], medicalConditions: [] };
        patientService.updateMedicalRecord.and.returnValue(of(null));
        component.saveAll();
        expect(patientService.updateMedicalRecord).toHaveBeenCalled();
        expect(router.navigate).toHaveBeenCalledWith(['/patient/medical-record', '']);
    });

    it('should handle error when failing to save medical record', () => {
        component.medicalRecord = { id: '1', patientId: '2',allergies: [], medicalConditions: [] };
        patientService.updateMedicalRecord.and.returnValue(throwError('error'));
        component.saveAll();
        expect(component.errorMessage).toBe('Failed to save medical record');
    });

    it('should navigate to patient medical record on cancel', () => {
        component.medicalRecordNumber = '123';
        component.cancelAll();
        expect(router.navigate).toHaveBeenCalledWith(['/patient/medical-record', '123']);
    });

    it('should delete an allergy', () => {
        const allergy = { id: '1', code: 'A1', designation: 'Allergy1', description: 'Description1' };
        component.medicalRecord = { id: '1', patientId: '2', allergies: [allergy], medicalConditions: [] };
        component.deleteAllergy(allergy);
        expect(component.medicalRecord.allergies.length).toBe(0);
    });

    it('should delete a medical condition', () => {
        const condition = { id: '1', code: 'C1', designation: 'Condition1', date: new Date() };
        component.medicalRecord = { id: '1', patientId: '2', allergies: [], medicalConditions: [condition] };
        component.deleteCondition(condition);
        expect(component.medicalRecord.medicalConditions.length).toBe(0);
    });

    it('should get all allergies', () => {
        const allergies: AllergiesView[] = [{ code: 'A1', designation: 'Allergy1' }];
        patientService.getAllAllergies.and.returnValue(of(allergies));
        component.getAllAllergies();
        expect(component.existingAllergies).toEqual(allergies);
    });

    it('should handle error when failing to get allergies', () => {
        patientService.getAllAllergies.and.returnValue(throwError('error'));
        component.getAllAllergies();
        expect(component.errorMessage).toBe('Failed to get allergies');
    });

    it('should get all medical conditions', () => {
        const conditions: MedicalConditionView[] = [{ code: 'C1', designation: 'Condition1'}];
        patientService.getAllAllMedicalConditions.and.returnValue(of(conditions));
        component.getAllMedicalConditions();
        expect(component.existingMedicalConditions).toEqual(conditions);
    });

    it('should handle error when failing to get medical conditions', () => {
        patientService.getAllAllMedicalConditions.and.returnValue(throwError('error'));
        component.getAllMedicalConditions();
        expect(component.errorMessage).toBe('Failed to get allergies');
    });

    it('should format date for input', () => {
        const date = new Date('2023-01-01');
        const formattedDate = component.formatDateForInput(date);
        expect(formattedDate).toBe('2023-01-01');
    });

    it('should update condition date', () => {
        const condition = { id: '1', code: 'C1', designation: 'Condition1', date: '' };
        component.updateConditionDate('2023-01-01', condition);
        expect(condition.date).toBe(new Date('2023-01-01').toISOString());
    });
});
