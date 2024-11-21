import { ComponentFixture, TestBed } from '@angular/core/testing';
import { Router } from '@angular/router';
import { of, throwError } from 'rxjs';
import { ListStaffsComponent } from './list-staffs.component';
import { StaffService } from '../../services/staff.service';
import { MessageService } from '../../services/message.service';
import { StaffsView } from '../../models/staff';

describe('ListStaffsComponent', () => {
  let component: ListStaffsComponent;
  let fixture: ComponentFixture<ListStaffsComponent>;
  let mockStaffService: jasmine.SpyObj<StaffService>;
  let mockRouter: jasmine.SpyObj<Router>;
  let mockMessageService: jasmine.SpyObj<MessageService>;

  beforeEach(async () => {
    mockStaffService = jasmine.createSpyObj('StaffService', ['getStaffs', 'filterStaffs']);
    mockRouter = jasmine.createSpyObj('Router', ['navigate']);
    mockMessageService = jasmine.createSpyObj('MessageService', ['getMessage']);

    await TestBed.configureTestingModule({
      imports: [ListStaffsComponent],
      providers: [
        { provide: StaffService, useValue: mockStaffService },
        { provide: Router, useValue: mockRouter },
        { provide: MessageService, useValue: mockMessageService }
      ]
    }).compileComponents();

    fixture = TestBed.createComponent(ListStaffsComponent);
    component = fixture.componentInstance;
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should fetch staffs on init', () => {
    const staffs: StaffsView[] = [{ id: '1', fullName: 'John Doe', licenseNumber: '123', phoneNumber: '1234567890', email: 'john@example.com', specializationId: '1', status: 'Active' }];
    mockStaffService.getStaffs.and.returnValue(of(staffs));
    mockMessageService.getMessage.and.returnValue('Success message');

    component.ngOnInit();

    expect(component.staffs).toEqual(staffs);
    expect(component.filteredStaffs).toEqual(staffs);
    expect(component.successMessage).toBe('Success message');
  });

  it('should handle error when fetching staffs', () => {
    const errorResponse = { error: { message: 'Error message' } };
    mockStaffService.getStaffs.and.returnValue(throwError(errorResponse));

    component.ngOnInit();

    expect(component.errorMessage).toBe('An error occurred while fetching staff members: Error message');
    expect(component.filteredStaffs).toEqual([]);
  });

  it('should navigate to create staff page', () => {
    component.createStaff();
    expect(mockRouter.navigate).toHaveBeenCalledWith(['/staff/create']);
  });

  it('should filter staffs', () => {
    const filter = { fullName: 'John', licenseNumber: '', phoneNumber: '', email: '', specializationId: '' };
    const filteredStaffs: StaffsView[] = [{ id: '1', fullName: 'John Doe', licenseNumber: '123', phoneNumber: '1234567890', email: 'john@example.com', specializationId: '1', status: 'Active' }];
    mockStaffService.filterStaffs.and.returnValue(of(filteredStaffs));

    component.onFilterChanged(filter);

    expect(component.filteredStaffs).toEqual(filteredStaffs);
  });

  it('should navigate to staff details page', () => {
    const staff: StaffsView = { id: '1', fullName: 'John Doe', licenseNumber: '123', phoneNumber: '1234567890', email: 'john@example.com', specializationId: '1', status: 'Active' };
    component.seeDetails(staff);
    expect(mockRouter.navigate).toHaveBeenCalledWith(['/staff/details', staff.id]);
  });

  it('should navigate to edit staff page', () => {
    const staff: StaffsView = { id: '1', fullName: 'John Doe', licenseNumber: '123', phoneNumber: '1234567890', email: 'john@example.com', specializationId: '1', status: 'Active' };
    component.editStaff(staff);
    expect(mockRouter.navigate).toHaveBeenCalledWith(['/staff/edit', staff.id]);
  });

  it('should navigate to delete staff page', () => {
    const staff: StaffsView = { id: '1', fullName: 'John Doe', licenseNumber: '123', phoneNumber: '1234567890', email: 'john@example.com', specializationId: '1', status: 'Active' };
    component.deleteStaff(staff);
    expect(mockRouter.navigate).toHaveBeenCalledWith(['/staff/delete', staff.id]);
  });
});