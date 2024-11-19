import { ComponentFixture, TestBed } from '@angular/core/testing';
import { ListStaffsComponent } from './list-staffs.component';
import { StaffService } from '../../services/staff.service';
import { MessageService } from '../../services/message.service';
import { Router } from '@angular/router';
import { of, throwError } from 'rxjs';
import { CommonModule } from '@angular/common';
import { FilterStaffsComponent } from '../filter-staffs/filter-staffs.component';

describe('ListStaffsComponent', () => {
  let component: ListStaffsComponent;
  let fixture: ComponentFixture<ListStaffsComponent>;
  let staffService: jasmine.SpyObj<StaffService>;
  let messageService: jasmine.SpyObj<MessageService>;
  let router: jasmine.SpyObj<Router>;

  beforeEach(async () => {
    const staffServiceSpy = jasmine.createSpyObj('StaffService', ['getStaffs', 'filterStaffs']);
    const messageServiceSpy = jasmine.createSpyObj('MessageService', ['getMessage']);
    const routerSpy = jasmine.createSpyObj('Router', ['navigate']);

    await TestBed.configureTestingModule({
      imports: [ListStaffsComponent, CommonModule, FilterStaffsComponent],
      providers: [
        { provide: StaffService, useValue: staffServiceSpy },
        { provide: MessageService, useValue: messageServiceSpy },
        { provide: Router, useValue: routerSpy }
      ]
    }).compileComponents();

    fixture = TestBed.createComponent(ListStaffsComponent);
    component = fixture.componentInstance;
    staffService = TestBed.inject(StaffService) as jasmine.SpyObj<StaffService>;
    messageService = TestBed.inject(MessageService) as jasmine.SpyObj<MessageService>;
    router = TestBed.inject(Router) as jasmine.SpyObj<Router>;
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should fetch staff on init', () => {
    const staff = [{ id: 'D202512344', name: 'staffMario', licenseNumber: '12346', phoneNumber: '+951999999998', email: 'emaill@gmail.com', specialization: 'Obstetricia', status: 'Active' }];
    staffService.getStaffs.and.returnValue(of(staff));
    messageService.getMessage.and.returnValue('Success message');

    component.ngOnInit();

    expect(component.staffs).toEqual(staff);
    expect(component.filteredStaffs).toEqual(staff);
    expect(component.successMessage).toBe('Success message');
    expect(component.totalPages).toBe(1);
  });

  it('should handle error when fetching staff', () => {
    const errorResponse = { error: { message: 'Error fetching staff' } };
    staffService.getStaffs.and.returnValue(throwError(errorResponse));

    component.ngOnInit();

    expect(component.staffs).toEqual([]);
    expect(component.filteredStaffs).toEqual([]);
    expect(component.errorMessage).toBe('An error occurred while fetching staff members: Error fetching staff');
  });

  it('should filter staff', () => {
    const filter = { name: 'staffMario', licenseNumber: '12346', phoneNumber: '+951999999998', email: 'emaill@gmail.com', specialization: 'Obstetricia' };
    const filteredStaff = [{ id: 'D202512344', name: 'staffMario', licenseNumber: '12346', phoneNumber: '+951999999998', email: 'emaill@gmail.com', specialization: 'Obstetricia', status: 'Active' }];
    staffService.filterStaffs.and.returnValue(of(filteredStaff));

    component.onFilterChanged(filter);

    expect(component.filteredStaffs).toEqual(filteredStaff);
    expect(component.totalPages).toBe(1);
    expect(component.currentPage).toBe(1);
  });

  it('should handle error when filtering staff', () => {
    const filter = { name: 'staffMario', licenseNumber: '12346', phoneNumber: '+951999999998', email: 'emaill@gmail.com', specialization: 'Obstetricia' };
    staffService.filterStaffs.and.returnValue(throwError('Error filtering staff'));

    component.onFilterChanged(filter);

    expect(component.filteredStaffs).toEqual([]);
  });

  it('should go to create staff page', () => {
    router.navigate.and.returnValue(Promise.resolve(true));
    component.createStaff();

    expect(router.navigate).toHaveBeenCalledWith(['/staff/create']);
  });

  it('should navigate to details page', () => {
    const staff = { id: 'D202512344', name: 'staffMario', licenseNumber: '12346', phoneNumber: '+951999999998', email: 'emaill@gmail.com', specialization: 'Obstetricia', status: 'Active' };
    router.navigate.and.returnValue(Promise.resolve(true));
    component.seeDetails(staff);

    expect(router.navigate).toHaveBeenCalledWith([`/staff/details/${staff.id}`]);
  });

  it('should delete staff', () => {
    const staff = { id: 'D202512344', name: 'staffMario', licenseNumber: '12346', phoneNumber: '+951999999998', email: 'emaill@gmail.com', specialization: 'Obstetricia', status: 'Active' };
    spyOn(window, 'confirm').and.returnValue(true);
    staffService.deleteStaff.and.returnValue(of('Staff deleted successfully'));

    component.deleteStaff(staff);

    expect(staffService.deleteStaff).toHaveBeenCalledWith(staff.id);
  });

  it('should handle error on delete staff', () => {
    const staff = { id: 'D202512344', name: 'staffMario', licenseNumber: '12346',phoneNumber: '+951999999998', email: 'emaill@gmail.com', specialization: 'Obstetricia', status: 'Active' };
    spyOn(window, 'confirm').and.returnValue(true);
    staffService.deleteStaff.and.returnValue(throwError('Error deleting staff'));

    component.deleteStaff(staff);

    expect(component.errorMessage).toBe('An error occurred while deleting staff: Error deleting staff');
  });
});
