import { ComponentFixture, TestBed } from '@angular/core/testing';
import { ActivatedRoute } from '@angular/router';
import { of, throwError } from 'rxjs';
import { StaffDetailsComponent } from './staff-details.component';
import { StaffService } from '../../services/staff.service';
import { CommonModule } from '@angular/common';
import { Category, StaffStatus } from '../../models/staff';

describe('StaffDetailsComponent', () => {
  let component: StaffDetailsComponent;
  let fixture: ComponentFixture<StaffDetailsComponent>;
  let staffService: jasmine.SpyObj<StaffService>;
  let route: ActivatedRoute;

  beforeEach(() => {
    const staffServiceSpy = jasmine.createSpyObj('StaffService', ['getStaffById']);
    route = { snapshot: { paramMap: { get: () => '1' } } } as any;

    TestBed.configureTestingModule({
      imports: [CommonModule, StaffDetailsComponent],
      providers: [
        { provide: StaffService, useValue: staffServiceSpy },
        { provide: ActivatedRoute, useValue: route }
      ]
    }).compileComponents();
    fixture = TestBed.createComponent(StaffDetailsComponent);
    component = fixture.componentInstance;
    staffService = TestBed.inject(StaffService) as jasmine.SpyObj<StaffService>;
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should fetch staff details on init', () => {
    const staffData = {
      id: 'D202512344',
      fullName: 'staffMario',
      licenseNumber: '12345',
      specialization: 'Obstetricia',
      email: 'emaill@gmail.com',
      phoneNumber: '+951999999998',
      category : Category.Doctor,
      status : StaffStatus.Active
    };
    staffService.getStaffById.and.returnValue(of(staffData));

    component.ngOnInit();

    expect(staffService.getStaffById).toHaveBeenCalledWith('D202512344');
    expect(component.staff).toEqual(staffData);
    expect(component.staff?.id).toBe('D202512344');
    expect(component.staff?.fullName).toBe('staffMario');
    expect(component.staff?.licenseNumber).toBe('12346');
    expect(component.staff?.specialization).toBe('Obstetricia');
    expect(component.staff?.email).toBe('emaill@gmail.com');
    expect(component.staff?.phoneNumber).toBe('+951999999998');
    expect(component.staff?.category).toBe(Category.Doctor);
    expect(component.staff?.status).toBe(StaffStatus.Active);
    expect(component.errorMessage).toBe('');
  });

  it('should handle error when fetching staff details', () => {
    staffService.getStaffById.and.returnValue(throwError('Error'));

    component.ngOnInit();

    expect(staffService.getStaffById).toHaveBeenCalledWith('1');
    expect(component.staff).toBeUndefined();
    expect(component.errorMessage).toBe('Failed to fetch staff details');
  });

  it('should set error message if id is not provided', () => {
    route.snapshot.paramMap.get = () => null;

    component.ngOnInit();

    expect(component.errorMessage).toBe('Invalid staff');
  });
});
