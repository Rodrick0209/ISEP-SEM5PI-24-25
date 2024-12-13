import { ComponentFixture, TestBed } from '@angular/core/testing';
import { EditMedicalConditionComponent } from './edit-medical-condition.component';
import { MedicalCondtionService } from '../../services/medicalConditions.service';
import { ActivatedRoute, Router } from '@angular/router';
import { MessageService } from '../../services/message.service';
import { of, throwError } from 'rxjs';
import { FormsModule } from '@angular/forms';
import { CommonModule } from '@angular/common';
import { MedicalCondition } from '../../models/patient';

describe('EditMedicalConditionComponent', () => {
  let component: EditMedicalConditionComponent;
  let fixture: ComponentFixture<EditMedicalConditionComponent>;
  let medicalConditionService: jasmine.SpyObj<MedicalCondtionService>;
  let router: jasmine.SpyObj<Router>;
  let messageService: jasmine.SpyObj<MessageService>;
  let route: ActivatedRoute;

  beforeEach(async () => {
    const medicalConditionServiceSpy = jasmine.createSpyObj('MedicalCondtionService', ['getMedicalConditionCatalogItem', 'updateMedicalConditionCatalogItem']);
    medicalConditionServiceSpy.getMedicalConditionCatalogItem.and.returnValue(of({ id: '1', name: 'testCondition', date: new Date() }));
    medicalConditionServiceSpy.updateMedicalConditionCatalogItem.and.returnValue(of({ id: '1', name: 'updatedCondition', date: new Date() }));
    const routerSpy = jasmine.createSpyObj('Router', ['navigate']);
    const messageServiceSpy = jasmine.createSpyObj('MessageService', ['setMessage']);

    await TestBed.configureTestingModule({
      imports: [FormsModule, CommonModule],
      providers: [
        { provide: MedicalCondtionService, useValue: medicalConditionServiceSpy },
        { provide: Router, useValue: routerSpy },
        { provide: MessageService, useValue: messageServiceSpy },
        { provide: ActivatedRoute, useValue: { snapshot: { paramMap: { get: () => 'testCondition' } } } }
      ]
    }).compileComponents();

    fixture = TestBed.createComponent(EditMedicalConditionComponent);
    component = fixture.componentInstance;
    medicalConditionService = TestBed.inject(MedicalCondtionService) as jasmine.SpyObj<MedicalCondtionService>;
    router = TestBed.inject(Router) as jasmine.SpyObj<Router>;
    messageService = TestBed.inject(MessageService) as jasmine.SpyObj<MessageService>;
    route = TestBed.inject(ActivatedRoute);

    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should initialize medical condition name on init', () => {
    const medicalCondition: MedicalCondition = { id: '1', name: 'testCondition', date: new Date() };
    medicalConditionService.getMedicalConditionCatalogItem.and.returnValue(of(medicalCondition));

    component.ngOnInit();

    expect(component.medicalConditionName).toBe('testCondition');
    expect(component.submitForm.name).toBe('testCondition');
  });

  it('should handle error when loading medical condition', () => {
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

  it('should update medical condition on submit', () => {
    const response: MedicalCondition = { id: '1', name: 'updatedCondition', date: new Date() };
    medicalConditionService.updateMedicalConditionCatalogItem.and.returnValue(of(response));

    component.medicalConditionName = 'testCondition';
    component.submitForm.name = 'updatedCondition';
    component.onSubmit({});

    expect(medicalConditionService.updateMedicalConditionCatalogItem).toHaveBeenCalledWith('testCondition', 'updatedCondition');
    expect(messageService.setMessage).toHaveBeenCalledWith('Medical Condition testCondition successfully edited!');
    expect(router.navigate).toHaveBeenCalledWith(['/medicalConditions']);
  });

  it('should handle error on update medical condition', () => {
    spyOn(console, 'error');
    const errorResponse = { error: { message: 'Update failed' } };
    medicalConditionService.updateMedicalConditionCatalogItem.and.returnValue(throwError(errorResponse));

    component.medicalConditionName = 'testCondition';
    component.submitForm.name = 'updatedCondition';
    component.onSubmit({});

    expect(component.errorMessage).toBe('Update failed');
    expect(console.error).toHaveBeenCalledWith('Failed to edit allergy', errorResponse);
  });

  it('should navigate to medical conditions on cancel', () => {
    component.onCancel();
    expect(router.navigate).toHaveBeenCalledWith(['/medicalConditions']);
  });
});