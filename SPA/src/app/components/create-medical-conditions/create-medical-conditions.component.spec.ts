import { ComponentFixture, TestBed } from '@angular/core/testing';
import { CreateMedicalConditionsComponent } from './create-medical-conditions.component';
import { MedicalCondtionService } from '../../services/medicalConditions.service';
import { MessageService } from '../../services/message.service';
import { Router } from '@angular/router';
import { of, throwError } from 'rxjs';
import { FormsModule } from '@angular/forms';

describe('CreateMedicalConditionsComponent', () => {
  let component: CreateMedicalConditionsComponent;
  let fixture: ComponentFixture<CreateMedicalConditionsComponent>;
  let medicalConditionService: jasmine.SpyObj<MedicalCondtionService>;
  let messageService: jasmine.SpyObj<MessageService>;
  let router: jasmine.SpyObj<Router>;

  beforeEach(async () => {
    const medicalConditionServiceSpy = jasmine.createSpyObj('MedicalCondtionService', ['createMedicalCondition']);
    const messageServiceSpy = jasmine.createSpyObj('MessageService', ['setMessage']);
    const routerSpy = jasmine.createSpyObj('Router', ['navigate']);

    await TestBed.configureTestingModule({
      imports: [FormsModule],
      providers: [
        { provide: MedicalCondtionService, useValue: medicalConditionServiceSpy },
        { provide: MessageService, useValue: messageServiceSpy },
        { provide: Router, useValue: routerSpy }
      ]
    }).compileComponents();

    fixture = TestBed.createComponent(CreateMedicalConditionsComponent);
    component = fixture.componentInstance;
    medicalConditionService = TestBed.inject(MedicalCondtionService) as jasmine.SpyObj<MedicalCondtionService>;
    messageService = TestBed.inject(MessageService) as jasmine.SpyObj<MessageService>;
    router = TestBed.inject(Router) as jasmine.SpyObj<Router>;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should add symptom', () => {
    component.newSymptom = 'Headache';
    component.addSymptom();
    expect(component.commonSymptoms).toContain('Headache');
    expect(component.newSymptom).toBe('');
  });

  it('should not add empty symptom', () => {
    component.newSymptom = ' ';
    component.addSymptom();
    expect(component.commonSymptoms.length).toBe(0);
    expect(component.errorMessage).toBe('Symptom cannot be empty');
  });

  it('should confirm submission', () => {
    component.confirmSubmission();
    expect(component.showConfirmation).toBeTrue();
  });

  it('should close confirmation modal', () => {
    component.closeConfirmationModal();
    expect(component.showConfirmation).toBeFalse();
  });

  it('should navigate to medical conditions on cancel', () => {
    component.onCancel();
    expect(router.navigate).toHaveBeenCalledWith(['/medicalConditions']);
  });

  it('should submit form successfully', () => {
    const form = { valid: true };
    const medicalCondition = { designation: 'Condition A' } as any;
    medicalConditionService.createMedicalCondition.and.returnValue(of(medicalCondition));

    component.onSubmit(form);

    expect(medicalConditionService.createMedicalCondition).toHaveBeenCalled();
    expect(messageService.setMessage).toHaveBeenCalledWith('Medical Condition Condition A successfully created!');
    expect(router.navigate).toHaveBeenCalledWith(['/medicalConditions']);
  });

  it('should handle form submission error', () => {
    const form = { valid: true };
    const errorResponse = { error: { message: 'Error occurred' } };
    medicalConditionService.createMedicalCondition.and.returnValue(throwError(errorResponse));

    component.onSubmit(form);

    expect(medicalConditionService.createMedicalCondition).toHaveBeenCalled();
    expect(component.errorMessage).toBe('An error occurred while creating the medical condition: Error occurred');
  });

  it('should not submit invalid form', () => {
    const form = { valid: false };

    component.onSubmit(form);

    expect(medicalConditionService.createMedicalCondition).not.toHaveBeenCalled();
  });
});