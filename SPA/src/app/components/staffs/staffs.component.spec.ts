
import { ComponentFixture, TestBed } from '@angular/core/testing';
import { StaffsComponent } from './staffs.component';
import { ListStaffsComponent } from '../list-staffs/list-staffs.component';
import { CreateStaffComponent } from '../create-staff/create-staff.component';
import { NO_ERRORS_SCHEMA } from '@angular/core';
import { StaffService } from '../../services/staff.service';
import { of } from 'rxjs/internal/observable/of';



describe('PatientsComponent', () => {
  let staffService: jasmine.SpyObj<StaffService>;
  let component: StaffsComponent;
  let fixture: ComponentFixture<StaffsComponent>;

  beforeEach(async () => {
    const staffServiceSpy = jasmine.createSpyObj('StaffService', ['getStaffs', 'filterStaffs']);
    staffServiceSpy.getStaffs.and.returnValue(of([]));
    await TestBed.configureTestingModule({
      imports: [StaffsComponent, ListStaffsComponent, CreateStaffComponent],
      providers: [
        { provide: StaffService, useValue: staffServiceSpy }
      ],
      schemas: [NO_ERRORS_SCHEMA]
    }).compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(StaffsComponent);
    component = fixture.componentInstance;
    staffService = TestBed.inject(StaffService) as jasmine.SpyObj<StaffService>;

    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should add a new staff to the list', () => {
    const newStaff = 'John Doe';
    component.onStaffAdded(newStaff);
    expect(component.staffs).toContain(newStaff);
  });

  it('should have an empty initial staff list', () => {
    expect(component.staffs.length).toBe(0);
  });
});
