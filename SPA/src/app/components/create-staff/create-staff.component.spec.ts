import { ComponentFixture, TestBed } from '@angular/core/testing';
import { Router } from '@angular/router';
import { of, throwError } from 'rxjs';
import { CreateStaffComponent } from './create-staff.component';
import { StaffService } from '../../services/staff.service';
import { MessageService } from '../../services/message.service';
import { FormsModule } from '@angular/forms';
import { CommonModule } from '@angular/common';

describe('CreateStaffComponent', () => {
  let component: CreateStaffComponent;
  let fixture: ComponentFixture<CreateStaffComponent>;
  let mockRouter: any;
  let mockStaffService: any;
  let mockMessageService: any;

  beforeEach(async () => {
    mockRouter = jasmine.createSpyObj('Router', ['navigate']);
    mockStaffService = jasmine.createSpyObj('StaffService', ['createStaff']);
    mockMessageService = jasmine.createSpyObj('MessageService', ['setMessage']);

    await TestBed.configureTestingModule({
      imports: [CreateStaffComponent, FormsModule, CommonModule],
      providers: [
        { provide: Router, useValue: mockRouter },
        { provide: StaffService, useValue: mockStaffService },
        { provide: MessageService, useValue: mockMessageService }
      ]
    }).compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(CreateStaffComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should show confirmation modal', () => {
    component.confirmSubmission();
    expect(component.showConfirmation).toBeTrue();
  });

  it('should close confirmation modal', () => {
    component.closeConfirmationModal();
    expect(component.showConfirmation).toBeFalse();
  });

  it('should navigate to staffs on cancel', () => {
    component.onCancel();
    expect(mockRouter.navigate).toHaveBeenCalledWith(['/staffs']);
  });

  it('should submit form successfully', () => {
    const staffForm = { valid: true };
    const staffData = {
      fullName: 'John Doe',
      licenseNumber: '12345',
      specialization: 'Cardiology',
      email: 'john.doe@example.com',
      phoneNumber: '1234567890',
      category: 'Doctor'
    };
    component.submitForm = staffData;
    mockStaffService.createStaff.and.returnValue(of({}));

    component.onSubmit(staffForm);

    expect(mockStaffService.createStaff).toHaveBeenCalledWith(
      staffData.fullName,
      staffData.licenseNumber,
      staffData.specialization,
      staffData.email,
      staffData.phoneNumber,
      staffData.category
    );
    expect(mockMessageService.setMessage).toHaveBeenCalledWith(`Staff ${staffData.fullName} successfully created!`);
    expect(mockRouter.navigate).toHaveBeenCalledWith(['/staffs']);
  });

  it('should handle form submission error', () => {
    const staffForm = { valid: true };
    const staffData = {
      fullName: 'John Doe',
      licenseNumber: '12345',
      specialization: 'Cardiology',
      email: 'john.doe@example.com',
      phoneNumber: '1234567890',
      category: 'Doctor'
    };
    component.submitForm = staffData;
    mockStaffService.createStaff.and.returnValue(throwError({ error: { message: 'Error' } }));

    component.onSubmit(staffForm);

    expect(mockStaffService.createStaff).toHaveBeenCalledWith(
      staffData.fullName,
      staffData.licenseNumber,
      staffData.specialization,
      staffData.email,
      staffData.phoneNumber,
      staffData.category
    );
    expect(component.errorMessage).toBe('Failed to create staff: Error');
  });

  it('should log invalid form', () => {
    spyOn(console, 'log');
    const staffForm = { valid: false };

    component.onSubmit(staffForm);

    expect(console.log).toHaveBeenCalledWith('Form is invalid');
  });
});