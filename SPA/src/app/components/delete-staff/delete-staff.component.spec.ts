import { ComponentFixture, TestBed } from '@angular/core/testing';
import { DeleteStaffComponent } from './delete-staff.component';
import { Router } from '@angular/router';
import { StaffService } from '../../services/staff.service';
import { MessageService } from '../../services/message.service';
import { ActivatedRoute } from '@angular/router';
import { CommonModule } from '@angular/common';
import { MarkXComponent } from '../template/mark-x/mark-x.component';
import { of, throwError } from 'rxjs';

describe('DeleteStaffComponent', () => {
  let component: DeleteStaffComponent;
  let fixture: ComponentFixture<DeleteStaffComponent>;
  let routerMock: any;
  let staffServiceMock: any;
  let messageServiceMock: any;
  let activatedRouteMock: any;

  beforeEach(async () => {
    routerMock = { navigate: jasmine.createSpy('navigate') };
    staffServiceMock = {
      deleteStaff: jasmine.createSpy('deleteStaff').and.returnValue(of({}))
    };
    messageServiceMock = {
      setMessage: jasmine.createSpy('setMessage')
    };
    activatedRouteMock = {
      snapshot: { paramMap: { get: jasmine.createSpy('get').and.returnValue('D202512344') } }
    };

    await TestBed.configureTestingModule({
      imports: [DeleteStaffComponent, CommonModule, MarkXComponent],
      providers: [
        { provide: Router, useValue: routerMock },
        { provide: StaffService, useValue: staffServiceMock },
        { provide: MessageService, useValue: messageServiceMock },
        { provide: ActivatedRoute, useValue: activatedRouteMock }
      ]
    }).compileComponents();

    fixture = TestBed.createComponent(DeleteStaffComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should set error message if not confirmed', () => {
    component.onDelete();
    expect(component.errorMessage).toBe('Please confirm the deletion');
  });

  it('should delete staff if confirmed', () => {
    component.isConfirmed = true;
    component.onDelete();
    expect(staffServiceMock.deleteStaff).toHaveBeenCalledWith('D2025123441');
    expect(messageServiceMock.setMessage).toHaveBeenCalledWith('Staff D202512344 successfully deleted!');
    expect(routerMock.navigate).toHaveBeenCalledWith(['/staffs']);
  });

  it('should handle error if delete fails', () => {
    staffServiceMock.deleteStaff.and.returnValue(throwError(() => new Error('Failed')));
    component.isConfirmed = true;
    component.onDelete();
    expect(component.errorMessage).toBe('Failed to delete staff');
  });

  it('should navigate on cancel', () => {
    component.onCancel();
    expect(routerMock.navigate).toHaveBeenCalledWith(['/staffs']);
  });
});
