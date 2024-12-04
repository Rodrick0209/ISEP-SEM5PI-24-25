import { ComponentFixture, TestBed } from '@angular/core/testing';
import { FormsModule } from '@angular/forms';
import { Router } from '@angular/router';
import { of, throwError } from 'rxjs';
import { AddRoomTypeComponent } from './add-room-type.component';
import { RoomTypeService } from '../../services/room-type.service';
import { MessageService } from '../../services/message.service';

describe('AddRoomTypeComponent', () => {
  let component: AddRoomTypeComponent;
  let fixture: ComponentFixture<AddRoomTypeComponent>;
  let roomTypeService: jasmine.SpyObj<RoomTypeService>;
  let router: jasmine.SpyObj<Router>;

  beforeEach(async () => {
    const roomTypeServiceSpy = jasmine.createSpyObj('RoomTypeService', ['add']);
    const routerSpy = jasmine.createSpyObj('Router', ['navigate']);

    await TestBed.configureTestingModule({
      imports: [FormsModule],
      providers: [
        { provide: RoomTypeService, useValue: roomTypeServiceSpy },
        { provide: Router, useValue: routerSpy },
        MessageService
      ]
    }).compileComponents();

    fixture = TestBed.createComponent(AddRoomTypeComponent);
    component = fixture.componentInstance;
    roomTypeService = TestBed.inject(RoomTypeService) as jasmine.SpyObj<RoomTypeService>;
    router = TestBed.inject(Router) as jasmine.SpyObj<Router>;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should show confirmation modal on confirmSubmission', () => {
    component.confirmSubmission();
    expect(component.showConfirmation).toBeTrue();
  });

  it('should close confirmation modal on closeConfirmationModal', () => {
    component.closeConfirmationModal();
    expect(component.showConfirmation).toBeFalse();
  });

  it('should call roomTypeService.add on valid form submission', () => {
    const roomTypeForm = { valid: true };
    const addRoomType = {
      InternalCode: '',
      Designation: '',
      Description: '',
      SuitableForSurgeries: false
    };

    roomTypeService.add.and.returnValue(of({}));

    component.onSubmit(roomTypeForm);

    expect(roomTypeService.add).toHaveBeenCalledWith(addRoomType);
  });

  it('should set successMessage on successful room type addition', () => {
    const roomTypeForm = { valid: true };
    roomTypeService.add.and.returnValue(of({}));

    component.onSubmit(roomTypeForm);

    expect(component.successMessage).toContain('succesfully added!');
  });

  it('should set errorMessage on failed room type addition', () => {
    const roomTypeForm = { valid: true };
    roomTypeService.add.and.returnValue(throwError({ error: { message: 'Error' } }));

    component.onSubmit(roomTypeForm);

    expect(component.errorMessage).toContain('Failed to add room type: Error');
  });

  it('should log form invalid message on invalid form submission', () => {
    spyOn(console, 'log');
    const roomTypeForm = { valid: false };

    component.onSubmit(roomTypeForm);

    expect(console.log).toHaveBeenCalledWith('Form is invalid');
  });

  it('should navigate back on cancel', () => {
    spyOn(history, 'back');

    component.onCancel();

    expect(history.back).toHaveBeenCalled();
  });
});