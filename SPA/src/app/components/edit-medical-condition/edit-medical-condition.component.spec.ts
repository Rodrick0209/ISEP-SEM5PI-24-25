import { ComponentFixture, TestBed } from '@angular/core/testing';
import { EditMedicalConditionComponent } from './edit-medical-condition.component';
import { MedicalCondtionService } from '../../services/medicalConditions.service';
import { MessageService } from '../../services/message.service';
import { ActivatedRoute, Router } from '@angular/router';
import { of, throwError } from 'rxjs';
import { FormsModule } from '@angular/forms';
import { CommonModule } from '@angular/common';

describe('EditMedicalConditionComponent', () => {
  let component: EditMedicalConditionComponent;
  let fixture: ComponentFixture<EditMedicalConditionComponent>;
  let medicalConditionService: jasmine.SpyObj<MedicalCondtionService>;
  let messageService: jasmine.SpyObj<MessageService>;
  let router: jasmine.SpyObj<Router>;
  let route: ActivatedRoute;

  beforeEach(async () => {
    const medicalConditionServiceSpy = jasmine.createSpyObj('MedicalCondtionService', ['getMedicalConditionCatalogItem', 'updateMedicalConditionCatalogItem']);
    const messageServiceSpy = jasmine.createSpyObj('MessageService', ['setMessage']);
    const routerSpy = jasmine.createSpyObj('Router', ['navigate']);

    await TestBed.configureTestingModule({
      imports: [FormsModule, CommonModule],
      providers: [
        { provide: MedicalCondtionService, useValue: medicalConditionServiceSpy },
        { provide: MessageService, useValue: messageServiceSpy },
        { provide: Router, useValue: routerSpy },
        { provide: ActivatedRoute, useValue: { snapshot: { paramMap: { get: () => 'testCondition' } } } }
      ]
    }).compileComponents();

    fixture = TestBed.createComponent(EditMedicalConditionComponent);
    component = fixture.componentInstance;
    medicalConditionService = TestBed.inject(MedicalCondtionService) as jasmine.SpyObj<MedicalCondtionService>;
    messageService = TestBed.inject(MessageService) as jasmine.SpyObj<MessageService>;
    router = TestBed.inject(Router) as jasmine.SpyObj<Router>;
    route = TestBed.inject(ActivatedRoute);
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should initialize with medical condition data', () => {
    const mockData = { code: 'A01', designation: 'Test Designation', description: 'Test Description', commonSymptoms: ['Test Symptom'] };
    medicalConditionService.getMedicalConditionCatalogItem.and.returnValue(of(mockData));

    component.ngOnInit();

    expect(component.submitForm.designation).toBe(mockData.designation);
    expect(component.submitForm.description).toBe(mockData.description);
  });

  it('should handle error when initializing with medical condition data', () => {
    spyOn(console, 'error');
    medicalConditionService.getMedicalConditionCatalogItem.and.returnValue(throwError({ error: 'Error' }));

    component.ngOnInit();

    expect(console.error).toHaveBeenCalledWith('Error loading medical condition', { error: 'Error' });
  });

  it('should show confirmation modal', () => {
    component.confirmSubmission();
    expect(component.showConfirmation).toBeTrue();
  });

  it('should close confirmation modal', () => {
    component.closeConfirmationModal();
    expect(component.showConfirmation).toBeFalse();
  });


  it('should handle error on submit', () => {
    spyOn(console, 'error');
    const mockError = { error: { message: 'Error' } };
    medicalConditionService.updateMedicalConditionCatalogItem.and.returnValue(throwError(mockError));

    component.onSubmit({});

    expect(component.errorMessage).toBe('Medical condition name is missing.');
  });

  it('should navigate to medical conditions on cancel', () => {
    component.onCancel();
    expect(router.navigate).toHaveBeenCalledWith(['/medicalConditions']);
  });
});