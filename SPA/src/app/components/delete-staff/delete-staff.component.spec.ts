import { ComponentFixture, TestBed } from '@angular/core/testing';
import { ActivatedRoute, Router } from '@angular/router';
import { of, throwError } from 'rxjs';
import { DeleteStaffComponent } from './delete-staff.component';
import { StaffService } from '../../services/staff.service';
import { MessageService } from '../../services/message.service';

describe('DeleteStaffComponent', () => {
  let component: DeleteStaffComponent;
  let fixture: ComponentFixture<DeleteStaffComponent>;
  let mockStaffService: jasmine.SpyObj<StaffService>;
  let mockMessageService: jasmine.SpyObj<MessageService>;
  let mockRouter: jasmine.SpyObj<Router>;
  let mockActivatedRoute: any;

  beforeEach(async () => {
    mockStaffService = jasmine.createSpyObj('StaffService', ['deleteStaff']);
    mockMessageService = jasmine.createSpyObj('MessageService', ['setMessage']);
    mockRouter = jasmine.createSpyObj('Router', ['navigate']);
    mockActivatedRoute = {
      snapshot: {
        paramMap: {
          get: jasmine.createSpy('get').and.returnValue('123')
        }
      }
    };

    await TestBed.configureTestingModule({
      providers: [
        { provide: StaffService, useValue: mockStaffService },
        { provide: MessageService, useValue: mockMessageService },
        { provide: Router, useValue: mockRouter },
        { provide: ActivatedRoute, useValue: mockActivatedRoute }
      ]
    }).compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(DeleteStaffComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should initialize id from route params', () => {
    expect(component.id).toBe('123');
  });

  it('should toggle isConfirmed on mark click', () => {
    expect(component.isConfirmed).toBeFalse();
    component.onMarkClick();
    expect(component.isConfirmed).toBeTrue();
    component.onMarkClick();
    expect(component.isConfirmed).toBeFalse();
  });

  it('should set errorMessage if delete is not confirmed', () => {
    component.onDelete();
    expect(component.errorMessage).toBe('Please confirm the deactivation');
  });

  it('should call deleteStaff and navigate on successful delete', () => {
    component.isConfirmed = true;
    mockStaffService.deleteStaff.and.returnValue(of({}));

    component.onDelete();

    expect(mockStaffService.deleteStaff).toHaveBeenCalledWith('123');
    expect(mockMessageService.setMessage).toHaveBeenCalledWith('Staff  123 successfully deactivated!');
    expect(mockRouter.navigate).toHaveBeenCalledWith(['/staffs']);
  });

  it('should set errorMessage on delete failure', () => {
    component.isConfirmed = true;
    mockStaffService.deleteStaff.and.returnValue(throwError('error'));

    component.onDelete();

    expect(mockStaffService.deleteStaff).toHaveBeenCalledWith('123');
    expect(component.errorMessage).toBe('Failed to deactivate staff');
  });

  it('should navigate to staffs on cancel', () => {
    component.onCancel();
    expect(mockRouter.navigate).toHaveBeenCalledWith(['/staffs']);
  });
});