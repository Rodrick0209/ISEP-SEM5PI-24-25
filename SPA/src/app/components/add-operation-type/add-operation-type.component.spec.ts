import { ComponentFixture, TestBed } from '@angular/core/testing';
import { FormsModule } from '@angular/forms';
import { AddOperationTypeComponent } from './add-operation-type.component';
import { NgForm } from '@angular/forms';
import { OperationTypesService } from '../../services/operation-type.service';
import { of, throwError } from 'rxjs';

describe('AddOperationTypeComponent', () => {
  let component: AddOperationTypeComponent;
  let fixture: ComponentFixture<AddOperationTypeComponent>;
  let operationTypeService: jasmine.SpyObj<OperationTypesService>;

  beforeEach(async () => {
    const spy = jasmine.createSpyObj('OperationTypesService', ['addOperationType']);

    await TestBed.configureTestingModule({
      imports: [FormsModule],
      providers: [{ provide: OperationTypesService, useValue: spy }]
    }).compileComponents();

    fixture = TestBed.createComponent(AddOperationTypeComponent);
    component = fixture.componentInstance;
    operationTypeService = TestBed.inject(OperationTypesService) as jasmine.SpyObj<OperationTypesService>;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should generate a GUID', () => {
    const guid = component['generateGUID']();
    expect(guid).toMatch(/^[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$/i);
  });

  it('should add preparation staff', () => {
    const initialLength = component.operationType.preparationPhase.requiredStaff.length;
    component.addPreparationStaff();
    expect(component.operationType.preparationPhase.requiredStaff.length).toBe(initialLength + 1);
  });

  it('should remove preparation staff', () => {
    component.addPreparationStaff();
    const initialLength = component.operationType.preparationPhase.requiredStaff.length;
    component.removePreparationStaff(0);
    expect(component.operationType.preparationPhase.requiredStaff.length).toBe(initialLength - 1);
  });

  it('should add surgery staff', () => {
    const initialLength = component.operationType.surgeryPhase.requiredStaff.length;
    component.addSurgeryStaff();
    expect(component.operationType.surgeryPhase.requiredStaff.length).toBe(initialLength + 1);
  });

  it('should remove surgery staff', () => {
    component.addSurgeryStaff();
    const initialLength = component.operationType.surgeryPhase.requiredStaff.length;
    component.removeSurgeryStaff(0);
    expect(component.operationType.surgeryPhase.requiredStaff.length).toBe(initialLength - 1);
  });

  it('should add cleaning staff', () => {
    const initialLength = component.operationType.cleaningPhase.requiredStaff.length;
    component.addCleaningStaff();
    expect(component.operationType.cleaningPhase.requiredStaff.length).toBe(initialLength + 1);
  });

  it('should remove cleaning staff', () => {
    component.addCleaningStaff();
    const initialLength = component.operationType.cleaningPhase.requiredStaff.length;
    component.removeCleaningStaff(0);
    expect(component.operationType.cleaningPhase.requiredStaff.length).toBe(initialLength - 1);
  });


  it('should submit the form if valid', () => {
    const form = { valid: true } as NgForm;
    operationTypeService.addOperationType.and.returnValue(of({
      id: '123',
      name: 'Test Operation',
      status: 'active',
      specialization: 'Test Specialization',
      preparationPhase: { requiredStaff: [], duration: 0 },
      surgeryPhase: { requiredStaff: [], duration: 0 },
      cleaningPhase: { requiredStaff: [], duration: 0 }
    }));

    component.onSubmit(form);

    expect(operationTypeService.addOperationType).toHaveBeenCalled();
    expect(component.errorMessage).toBeNull();
  });

  it('should not submit the form if invalid', () => {
    const form = { valid: false } as NgForm;

    component.onSubmit(form);

    expect(operationTypeService.addOperationType).not.toHaveBeenCalled();
    expect(component.errorMessage).toBe('There was an error submitting the new Operation Type');
  });

  it('should handle error on form submission', () => {
    const form = { valid: true } as NgForm;
    operationTypeService.addOperationType.and.returnValue(throwError(() => new Error('Error')));

    component.onSubmit(form);

    expect(operationTypeService.addOperationType).toHaveBeenCalled();
    expect(component.errorMessage).toBeNull();
  });

  it('should cancel the form', () => {
    spyOn(component.formClosed, 'emit');

    component.onCancel();

    expect(component.formClosed.emit).toHaveBeenCalled();
  });
});